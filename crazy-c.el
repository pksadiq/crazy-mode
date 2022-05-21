;;; crazy-c.el --- Crazy ways to code  -*- lexical-binding: t; -*-

;; Author: Mohammed Sadiq <sadiq@sadiqpk.org>

;; This file is NOT part of GNU Emacs.

;; To the extent possible under law, the author(s) have dedicated all
;; copyright and related and neighboring rights to this software to
;; the public domain worldwide.  This software is distributed WITHOUT
;; ANY WARRANTY.

;; You should have received a copy of the CC0 Public Domain Dedication
;; along with this software.  If not, see
;; <http://creativecommons.org/publicdomain/zero/1.0/>.

;;; Code:

(require 'cl-lib)

(defvar-local crazy-c-last-inhibited nil)

(defun crazy-c-skip-or-insert (char)
  (if (eq (following-char) char)
      (forward-char)
    (insert-char char)))

(defun crazy-c-electric-pair-inhibit-predicate (char)
  (cond
   ((and (eq char ?\{)
         (looking-at-p "[ \t]*\n?[ \t]*{"))
    (setq crazy-c-last-inhibited ?\{)
    t)
   (t (c-electric-pair-inhibit-predicate char))
   ))

(cl-defun crazy-c-defun-decl-arg-start ()
  ;; return the start point of function declaration if any
  ;; nil otherwise
  (let ((paran-list (nth 9 (syntax-ppss))))
    (cond
     ((or (null paran-list)
          (> (length paran-list) 4))
      nil)
     (t
      (cl-dolist (pos paran-list)
        (save-excursion
          (goto-char pos)
          ;; FIXME: return nil on error
          (forward-sexp)
          (if (or (eq (caar (c-point-syntax)) 'func-decl-cont)
                  ;; FIXME: required for functions followed by macro calls
                  ;; that don't end with ';' like G_DEFINE_TYPE
                  (eq (caar (c-point-syntax)) 'knr-argdecl-intro))
              (cl-return pos)))))
     )))

(cl-defun crazy-c-defun-call-start ()
  ;; Return the start point of the function call, the point where '(' is
  (let ((paran-list (nth 9 (syntax-ppss))))
    (cond
     ((null paran-list)
      nil)
     (t
      (cl-dolist (pos paran-list)
        (save-excursion
          (goto-char pos)
          ;; FIXME: return nil on error
          (forward-sexp)
          (backward-char)
          (if (or (eq (caar (c-point-syntax)) 'func-decl-cont)
                  ;; FIXME: required for functions followed by macro calls
                  ;; that don't end with ';' like G_DEFINE_TYPE
                  (eq (caar (c-point-syntax)) 'knr-argdecl-intro))
              (cl-return pos)))))
     )))

(defun crazy-c-str-to-snake (str)
  ;; Convert STR like 'snake style', SnakeStyle', 'snake-style'
  ;; 'SNAKE_STYLE', etc. to snake_style
  (let ((case-fold-search nil))
    (setq str (or str ""))
    (when (string-match-p "\\([A-Z][_ -]?[A-Z]\\)\\{2,\\}" str)
      (setq str (downcase str)))
    (when (string-match-p "^[A-Z_0-9 -]*$" str)
      (setq str (downcase str)))
    (setq str (replace-regexp-in-string "\\([a-zA-Z_]\\)\\([A-Z]\\)" "\\1_\\2" str))
    (downcase (replace-regexp-in-string "[- ]" "_" str))
    ))

(defun crazy-c-str-to-style (str style)
  (let ((case-fold-search nil)
        (new-str (crazy-c-str-to-snake str)))
    (cond
     ((string= style "snake")
      new-str)
     ((string= style "upsnake")
      (upcase new-str))
     ((string= style "upcamel")
      (setq new-str (upcase-initials new-str))
      (replace-regexp-in-string "\\(.\\)_\\([^_]\\)" "\\1\\2" new-str))
     ((string= style "lisp")
      (replace-regexp-in-string "_" "-" new-str))
     ((string= style "cycle")
      (cond
       ;; Upcase if in lower snake case
       ((string-match-p "^[-_a-z0-9]*$" str)
        (upcase new-str))
       ;; CamelCase if in upper snake case
       ((string-match-p "^[-_A-Z0-9]*$" str)
        (crazy-c-str-to-style str "upcamel"))
       ;; If 4 consecutive upcase, return upper snake case
       ((string-match-p "[A-Z]\\{4\\}" str)
        (upcase new-str))
       (t new-str)))
     )))

(defun crazy-c-be-sane ()
  (cond
   ((or (nth 4 (syntax-ppss))
        (and (nth 3 (syntax-ppss))
             ;; don't be sane in #include "|"
             (not (crazy-c-in-include-fname-p)))
        )
    t)
   ((and (eq (preceding-char) ?\s)
	 (eq (char-before (1- (point))) ?\')
	 (save-excursion
	   (nth 3 (syntax-ppss (- (point) 2)))))
    t)
   (t nil)
   ))

(defun crazy-c-in-include-fname-p ()
  (let ((start (nth 9 (syntax-ppss)))
        (string-start (nth 8 (syntax-ppss))))
    (setq start (car (last start)))
    (cond
     ((or
       (null (setq start (or start string-start)))
       (not (memq (char-after start) '(?\< ?\")))
       (< start (line-beginning-position))
       (> start (line-end-position))
       (< (point) start))
      nil)
     ((save-excursion
        (beginning-of-line)
        (if (looking-at-p c-cpp-include-key)
            t)))
     (t nil)
     )))

(defun crazy-c-dwim-space ()
  (delete-char -1)
  (cond
   ;; In #include <|> insert '-'
   ((crazy-c-in-include-fname-p)
    (if (eq (preceding-char) ?\-)
        (progn
          ;; if it's already a '-' preceding, replace it with '_'
          (delete-char -1)
          (insert "_"))
      (insert "-")))
   ;; stuff like time_t size_t
   ((looking-back "_t" 2)
    (insert " "))
   ((and
     (eq (char-after (line-beginning-position)) ?\#)
     (looking-back "^#[ ]*[a-zA-Z0-9]*" (line-beginning-position)))
    (insert " "))
   ;; do real space after certain chars
   ((looking-back "[,:;){}#<>&|/}]" 1)
    (insert " "))
   ((and (eq (preceding-char) ?\*)
         (eq (following-char) ?\))
         (or (looking-back (concat "(\\b" c-primitive-type-key) (line-beginning-position))
             (looking-back "(\\(const \\)?[A-Z]+[a-z]* \\*" (line-beginning-position))))
    (forward-char))
   ;; in "if SPC" etc, do "if (|)"
   ((looking-back "[ \t]\\(if\\|for\\|while\\|switch\\) ?" (line-beginning-position))
    (delete-horizontal-space t)
    (crazy-c-skip-or-insert ?\s)
    (if (eq (following-char) ?\()
        (forward-char)
      (setq last-command-event ?\()
      (c-electric-paren nil)))
   ;; replace _SPC with ->
   ((eq (preceding-char) ?\_)
    (delete-char -1)
    (insert "->"))
   ;; on SPC SPC)) do ) SPC )
   ((and (eq (preceding-char) ?\s)
         (memq (following-char) '(?\) ?\] ?\})))
    (delete-char -1)
    (progn
      ;; on ", SPC)" do "),SPC"
      (let ((was-comma (eq (preceding-char) ?\,)))
        (when was-comma (delete-char -1))
        (forward-char)
        (when was-comma (insert ","))))
    (insert " "))
   ;; Insert real space on SPC after numbers
   ((string-match-p "^[0-9]" (or (current-word) ""))
    (insert " "))
   ((looking-back "[a-zA-Z0-9]" 1)
    (let ((case-fold-search nil))
      (if (string-match-p (concat "^" c-keywords-regexp) (current-word))
          (insert " ")
        (insert "_"))))
   (t
    (insert "_"))))

(defun crazy-c-dwim-in-string ()
  (cond
   ((eq last-command-event ?\,)
    ;; insert a space after ','
    (if (looking-at-p " ?\"")
        (if (eq (following-char) ?\s)
            (forward-char)
          (insert " "))))
   ((eq last-command-event ?\s)
    ;; replace ", SPC" with "",SPC
    (when (and (eq (following-char) ?\")
               (looking-back ",  " 3))
      (delete-char -3)
      (forward-char)
      (crazy-c-skip-or-insert ?\,)
      (crazy-c-skip-or-insert ?\s)))
   ))

(defun crazy-c-dwim-dot ()
  (delete-char -1)
  (cond
   ((and (bolp) (eolp))
    (insert "#"))
   ;; Replace (.) with ... when appropriate
   ((and (looking-at-p "))")
         (looking-back "[,(] ?(" 3)
         (or (crazy-c-defun-decl-arg-start)
             (save-excursion
               (forward-char 2)
               (backward-sexp)
               (backward-char)
               (eq (caar (c-point-syntax)) 'cpp-define-intro))))
    (delete-char -1)
    (delete-char 1)
    (insert "..."))
   ((and (looking-at-p "[ \t]*$")
         (looking-back "[;}]\n[ \t]*" (max (- (line-beginning-position) 2) 0)))
    (delete-region (line-beginning-position) (line-end-position))
    (delete-char -1)
    (forward-char)
    (indent-according-to-mode))
   ;; replace #include <.> with #include "|" or vice versa
   ((and (save-excursion
           (backward-char)
           (looking-at "<>\\|\"\""))
         (crazy-c-in-include-fname-p))
    (backward-char)
    (if (eq (following-char) ?\<)
        (insert "\"\"")
      (insert "<>"))
    (delete-char 2)
    (backward-char))
   ((and (looking-at-p "}")
         (looking-back "^[ \t]*" (line-beginning-position)))
    (forward-char))
   ;; replace ..| with (|)
   ((eq (preceding-char) ?\.)
    (delete-char -1)
    (if (looking-back c-cpp-include-key (line-beginning-position))
        (progn
          (unless (eq (preceding-char) ?\ )
            (insert " "))
          (insert "<>")
          (backward-char 1))
      (setq last-command-event ?\()
      ;; FIXME: (c-electric-paren) appends additional ')' in macros
      (if (and (save-excursion (c-beginning-of-macro))
               (bound-and-true-p electric-pair-mode))
          (electric-pair--insert ?\()
        (c-electric-paren nil))
      ))
   ;; we didn’t handle.  Insert deleted ‘.’
   (t
    (insert-char ?\.))))

(defun crazy-c-dwim-comma ()
  (cond
   ;; Replace ,, to =
   ((looking-back ", ?," 3)
    (delete-char -1)
    (delete-horizontal-space t)
    (delete-char -1)
    (when (looking-back "[a-zA-Z0-9_)]\\|[]]" 1)
      (crazy-c-skip-or-insert ?\s))
    (insert "=")
    (crazy-c-skip-or-insert ?\s))
   ;; Replace =, to ==
   ((looking-back "[^=]= ?," 4)
    (delete-char -1)
    (delete-horizontal-space t)
    (insert "=")
    (if (eq (following-char) ?\s)
        (forward-char)
      (insert " ")))
   ((eq (following-char) ?\])
    (delete-char -1)
    (forward-char)
    (crazy-c-skip-or-insert ?\,)
    (crazy-c-skip-or-insert ?\s))
   ;; replace "== ," with "==, "
   ((looking-back "== ," 4)
    (transpose-chars -1)
    (forward-char))
   ;; default: Insert a space after ","
   (t (crazy-c-skip-or-insert ?\s))
   ))

(defun crazy-c-dwim-quote ()
  (cond
   ((looking-back "[a-zA-Z_0-9]'" 2)
    (delete-char -1)
    (save-excursion
      (search-backward (current-word))
      (replace-match (crazy-c-str-to-style (current-word) "cycle") t)))
   ))

(defun crazy-c-dwim-colon ()
  (let ((case-fold-search nil))
    (cond
     ((looking-back "\\bcase .*:" (line-beginning-position))
      nil)
     ((string-match-p (concat "^" c-primitive-type-key) (or (current-word) ""))
      (delete-char -1)
      (crazy-c-skip-or-insert ?\s)
      (insert "*"))
     ((looking-back "_t:" 3)
      (delete-char -1)
      (insert " *")
      (indent-according-to-mode))
     ;; Replace a string with _ as upcamel, eg.: "g_uri:" gives "GUri *"
     ((and (looking-back "[a-zA-Z0-9_]:" 2)
           (string-match-p "^[a-z]+_[a-z0-9_]+$" (or (current-word) "")))
      (delete-char -1)
      (search-backward (current-word))
      (replace-match (crazy-c-str-to-style (current-word) "upcamel") t)
      (insert " *")
      (indent-according-to-mode))
     )))

(defun crazy-c-dwim-semi-colon ()
  (cond
   ;; On header file names create a new line and move to it
   ((crazy-c-in-include-fname-p)
    (delete-char -1)
    (goto-char (line-end-position))
    (if (looking-at-p "\n\n")
        (forward-char)
      (insert "\n")))
   ((crazy-c-defun-decl-arg-start)
    (delete-char -1)
    (progn
      (let ((pos (crazy-c-defun-decl-arg-start)))
        (save-excursion
          (goto-char pos)
          ;; add 'void' string if empty arg list
          (when (looking-at-p "()")
            (forward-char)
            (insert "void")))
        (goto-char pos)
        (forward-sexp)
        (save-excursion
          (backward-char)
          (when (bound-and-true-p gnome-c-style-mode)
            (gnome-c-align-arglist-at-point)))
        (if (eq (following-char) ?\;)
            (forward-char)
          (setq last-command-event ?\;)
          (c-electric-semi&comma nil)))))
   ((and (looking-at-p "\\([])]\\)\\|\\()\\)")
         (not (save-excursion
                (forward-char)
                (c-after-conditional))))
    (delete-char -1)
    (while (memq (following-char) '(?\) ?\]))
      (forward-char))
    (if (eq (following-char) ?\;)
        (forward-char)
      (setq last-command-event ?\;)
      (c-electric-semi&comma nil)))
   ))

(defun crazy-c-dwim-brace ()
  ;; If we already have a '{' move to that, instead of adding
  (when (eq crazy-c-last-inhibited ?\{)
    (delete-region (save-excursion (c-backward-token-1) (point)) (point))
    (when (and (eq (preceding-char) ?\n)
               (eq (following-char) ?\n))
      (delete-char 1)
      ;; Move into '{'
      (forward-char 1)
      ;; go to next line and indent if possible
      (when (looking-at-p "[ \t]*\n[ \t]*\n")
        (delete-region (point) (line-end-position))
        (forward-char)
        (indent-according-to-mode))))
  (cond
   ((and (not (eq crazy-c-last-inhibited ?\{))
         (save-excursion
           (c-backward-sws)
           (eq (caar (c-point-syntax)) 'func-decl-cont)))
    (setq last-command-event ?\{)
    (c-electric-brace nil))
   ((crazy-c-defun-decl-arg-start)
    (progn
      (let ((pos (crazy-c-defun-decl-arg-start)))
        (save-excursion
          (backward-char)
          ;; We don't inhibit '{' electric as it results in sexp error
          (when (looking-at-p "{}")
            (delete-char 2))
          (goto-char pos)
          ;; Insert "void" for empty function arg list
          (when (looking-at-p "()")
            (forward-char)
            (insert "void")))
        (goto-char pos)
        (forward-sexp)
        (save-excursion
          (backward-char)
          (when (bound-and-true-p gnome-c-style-mode)
            (gnome-c-align-arglist-at-point)))
        (setq last-command-event ?\{)
        (c-electric-brace nil))))
   )
  (setq crazy-c-last-inhibited nil)
)

(defun crazy-c-dwim-square-bracket ()
  (cond
   ((save-excursion
      (looking-at-p "]"))
    (delete-char -1)
    (forward-char 1)
    (insert "[]")
    (backward-char 1))
))

(defun crazy-c-try-crazy ()
  (cond
   ((and (nth 3 (syntax-ppss))
         (not (crazy-c-in-include-fname-p)))
    (crazy-c-dwim-in-string))
   ((crazy-c-be-sane)
    t)
   ((eq last-command-event ?\.)
    (crazy-c-dwim-dot))
   ((eq last-command-event ?\,)
    (crazy-c-dwim-comma))
   ((eq last-command-event ?\s)
    (crazy-c-dwim-space))
   ((eq last-command-event ?\')
    (crazy-c-dwim-quote))
   ((eq last-command-event ?\:)
    (crazy-c-dwim-colon))
   ((eq last-command-event ?\;)
    (crazy-c-dwim-semi-colon))
   ((eq last-command-event ?\{)
    (crazy-c-dwim-brace))
   ((eq last-command-event ?\[)
    (crazy-c-dwim-square-bracket))
   ))

(provide 'crazy-c)
;;; crazy-c.el ends here

;;; crazy-elisp.el --- Crazy ways to code  -*- lexical-binding: t; -*-

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

(defun crazy-elisp-be-sane ()
  ;; situations to avoid being crazy
  (cond
   ;; comments and strings
   ((or (nth 4 (syntax-ppss))
	(nth 3 (syntax-ppss)))
    t)
   (t nil)))

(defun crazy-elisp-dwim-space ()
  (delete-char -1)
  (cond
   ;; Replace - in )-, "- with SPC
   ((memq (preceding-char) '(?\) ?\"))
    (insert " "))
   ;; replace ,SPC with ,@
   ((eq (preceding-char) ?\,)
    (insert "@"))
   (t
    (insert  "-"))))

(defun crazy-elisp-dwim-dot ()
  (delete-char -1)
  (cond
     ;; replace ..| with (|)
     ((eq (preceding-char) ?\.)
      (delete-char -1)
      (unless (or (bolp)
                  (memq (preceding-char) '(?\s ?\) ?\' ?\` ?\, ?\@)))
        (insert-char ?\s))
      (insert "()")
      (backward-char 1))
     ((and (looking-back "`?((" 3)
           (looking-at-p "))"))
      (if (looking-back "`((" 3)
          (insert ".")
        (save-excursion
          (backward-char 2)
          (unless (eq (preceding-char) ?\`)
            (insert "`"))
          )))
     ;; replace (.| with `(| and `(. with (()
     ((eq (preceding-char) ?\()
      (if (save-excursion
            (backward-char)
            (eq (preceding-char) ?\`))
          (progn
            (delete-char -2)
            (insert "(()")
            (backward-char))
        (save-excursion
          (backward-char)
          (insert "`"))))
     ;; we didn’t handle.  Insert deleted ‘.’
     (t
      (insert-char ?\.))))

(defun crazy-elisp-dwim-comma ()
  (delete-char -1)
  (cond
   ;; In ",|)" do ")|"
   ((eq (following-char) ?\))
    (forward-char))
   (t (insert ","))
   ))

(defun crazy-elisp-dwim-quote ()
  (cond
   ((looking-back " '" 2)
    ;; do nothing
    nil)
   ((looking-back "[a-zA-Z0-9-]'" 1)
    (save-excursion
      (backward-char)
      (insert " ")))
   ))

(defun crazy-elisp-try-crazy ()
  (cond
   ((crazy-elisp-be-sane)
    t)
   ((eq last-command-event ?\ )
    (crazy-elisp-dwim-space))
   ((eq last-command-event ?\.)
    (crazy-elisp-dwim-dot))
   ((eq last-command-event ?\,)
    (crazy-elisp-dwim-comma))
   ((eq last-command-event ?\')
    (crazy-elisp-dwim-quote))
   ))

(provide 'crazy-elisp)
;;; crazy-elisp.el ends here

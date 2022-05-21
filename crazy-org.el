;;; crazy-org.el --- Crazy ways to code  -*- lexical-binding: t; -*-

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

(defun crazy-org-dwim-space ()
  (delete-char -1)
  (cond
   ;; Save some CPU for really large lines.
   ;; XXX: Why can't we download more cores instead?
   ((> (- (point) (line-beginning-position)) 32)
    t)
   ((looking-back "^[*]*[ ]?" (line-beginning-position))
    (if (looking-back "^[ ]+.*" (line-beginning-position))
        (insert " ")
      (delete-horizontal-space t)
      (insert "*")
      (if (eq (following-char) ?\s)
          (forward-char)
        (insert " ")))
    (undo-boundary))
   ;; Replace - SPC SPC with - [ ] SPC
   ((and (eolp) (org-in-item-p) (looking-back "^[ ]*[-] " (line-beginning-position)))
    (delete-horizontal-space t)
    (insert " [ ] ")
    (org-update-checkbox-count))
   (t (insert " "))
   ))

(defun crazy-org-dwim-dot ()
  (cond
   ((> (- (point) (line-beginning-position)) 3)
    t)
   ;; replace #SPC with #+
   ((and (eolp) (looking-back "^#." (line-beginning-position)))
    (delete-char -1)
    (insert "+"))
   ;; replace ^SPC with #
   ((and (eolp) (looking-back "^." (line-beginning-position)))
    (delete-char -1)
    (insert "#"))
   ((and (looking-back "^[ \t]+." (line-beginning-position))
         (looking-at-p "[ \t]*[^-]"))
    (delete-char -1)
    (insert "- "))
   ))

(defun crazy-org-try-crazy ()
  (cond
   ((eq last-command-event ?\s)
    (crazy-org-dwim-space))
   ((eq last-command-event ?\.)
    (crazy-org-dwim-dot))
   ))

(provide 'crazy-org)
;;; crazy-org.el ends here

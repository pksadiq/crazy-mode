;;; crazy.el --- Crazy ways to code  -*- lexical-binding: t; -*-

;; Author: Mohammed Sadiq <sadiq@sadiqpk.org>

;; Version: 0.0.1
;; Author: Mohammed Sadiq <sadiq@sadiqpk.org>
;; URL: https://sadiqpk.org/projects/crazy-mode.html
;; Package-Requires: ((emacs "27"))
;; Keywords: convenience, hacks

;; This file is NOT part of GNU Emacs.

;; To the extent possible under law, the author(s) have dedicated all
;; copyright and related and neighboring rights to this software to
;; the public domain worldwide.  This software is distributed WITHOUT
;; ANY WARRANTY.

;; You should have received a copy of the CC0 Public Domain Dedication
;; along with this software.  If not, see
;; <http://creativecommons.org/publicdomain/zero/1.0/>.

;;; Commentary:
;; This minor modes adds various dwim keyboard shortcuts to your Emacs.

;;; Code:

(require 'crazy-c)
(require 'crazy-elisp)
(require 'crazy-org)

(defvar-local crazy-old-electric-inhibit nil)

(defun crazy-insert-space ()
  (interactive)
  (insert " "))

(defun crazy-be-crazy? ()
  ;; TODO
  t)

(defun crazy-electric-pair-inhibit-predicate (char)
  (cond
   ((derived-mode-p 'c-mode)
    (crazy-c-electric-pair-inhibit-predicate char))
   (t
    nil)))

(defun crazy-be-really-crazy ()
  (cond
   ((derived-mode-p 'c-mode)
    (crazy-c-try-crazy))
   ((derived-mode-p 'emacs-lisp-mode)
    (crazy-elisp-try-crazy))
   ;; TODO
   ;; ((derived-mode-p 'org-mode)
   ;;  (crazy-org-try-crazy))
))

(defun crazy-post-self-insert-function ()
  (when (crazy-be-crazy?)
    (crazy-be-really-crazy)))

(define-minor-mode crazy-mode
  "Toggle Crazy ways for coding (Crazy mode).

With a prefix argument ARG, enable crazy mode if ARG is positive, and
disable it otherwise.  If called from Lisp, enable the mode if ARG is
omitted or nil.

This mode shall allow you to code in crazy ways.  Currently only
‘c-mode’ is supported (other modes may be supported, but untested).

Please note: If you don’t know what this mode does, this mode shall
drive you crazy, rather than helping you code crazy.

This is a local minor mode."
  :global nil
  :lighter " !"
  :group 'crazy
  :keymap (let ((map (make-sparse-keymap)))
            (define-key map (kbd "S-SPC") 'crazy-insert-space)
            map)
  (if crazy-mode
      (progn
        (add-hook 'post-self-insert-hook
        	  #'crazy-post-self-insert-function nil t)
        (when (boundp 'electric-pair-inhibit-predicate)
          (make-local-variable 'electric-pair-inhibit-predicate)
          (setq crazy-old-electric-inhibit electric-pair-inhibit-predicate)
          (setq electric-pair-inhibit-predicate
	        #'crazy-electric-pair-inhibit-predicate)))
    (progn
      (remove-hook 'post-self-insert-hook
                   #'crazy-post-self-insert-function))
    (when (boundp 'electric-pair-inhibit-predicate)
      (make-local-variable 'electric-pair-inhibit-predicate)
      (setq electric-pair-inhibit-predicate
	    crazy-old-electric-inhibit))))

(provide 'crazy)
;;; crazy.el ends here

;;; org-roam-peek.el --- Peek into nodes with posframe.

;; Copyright (C) 2022 Dominik Keller.


;; Author: Dominik Keller
;; Version: 0.1.0
;; Package-Requires: ((emacs "26.0")(posframe "1.0.0")(helm "2.0.0")(org "9.4"))
;; Keywords: org-mode, roam, convenience, posframe
;; URL: https://www.github.com/domse007/org-roam-peek

;; This file is NOT part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:
;; Show a posframe to see inside a node without following it.

;;; Code:
(require 'posframe)
(require 'org-roam)
(require 'org)

;;; Options:
(defgroup org-roam-peek nil
  "Using posframe to peek into org-roam links."
  :prefix "org-roam-peek"
  :group 'convenience)

(defcustom org-roam-peek-posframe-poshandler
  #'posframe-poshandler-frame-top-right-corner
  "Poshandler for the posframe. posframe-poshandler-* family of functions are
alternatives"
  :group 'org-roam-peek
  :type 'function)

(defcustom org-roam-peek-posframe-border-width 1
  "The widht of the border of the posframe."
  :group 'org-roam-peek
  :type 'number)

(defcustom org-roam-peek-posframe-width (lambda () (/ (frame-width) 5))
  "The width of the posframe."
  :group 'org-roam-peek
  :type '(choice number function))

(defcustom org-roam-peek-posframe-height (lambda () (/ (frame-height) 4))
  "The height of the posframe."
  :group 'org-roam-peek
  :type '(choice number function))

(defcustom org-roam-peek-buffer-name "*org-roam-peek*"
  "Name of the buffer that is shown in the posframe."
  :group 'org-roam-peek
  :type 'number)

(defface org-roam-peek-posframe-border
  '((t (:inherit default :background "gray50")))
  "Border color of the org-roam-peek-buffer."
  :group 'org-roam-peek)

;;; Variables:
(defvar org-roam-peek--enabled nil
  "Wheter the mode is enabled or not.")

(defvar org-roam-peek--shown nil
  "Indicator if a buffer is shown.")

(defvar org-roam-peek--last-id ""
  "Id of the last visible id.")

(defvar org-roam-peek--link-types '("id" "roam")
  "List of types on which org-roam-peek will acto on.")

;;; Functions:
(defun org-roam-peek--post-cmd ()
  "Function that is called to see if the cursor is hovering over a link. If
that is the case, show the preview."
  (let* ((object (org-element-context))
         (type (car object))
         (link-info (car (cdr object))))
    (when (and (eq type 'link) (not org-roam-peek--shown))
      (let ((link-type (plist-get link-info :type))
            (link-id (plist-get link-info :path)))
        (when (member link-type org-roam-peek--link-types)
          (get-buffer-create org-roam-peek-buffer-name)
          (with-current-buffer org-roam-peek-buffer-name
	    ;; must only be reloaded if the id has changed.
	    (unless (string-equal org-roam-peek--last-id link-id)
	      (delete-region (point-min) (point-max))
	      (insert-file-contents (car (org-roam-id-find link-id nil)))
	      (let ((inhibit-message t))(org-mode))) ; supress minibuffer output
	    (org-roam-peek--show-posframe org-roam-peek-buffer-name))
	  (setq org-roam-peek--shown t
		org-roam-peek--last-id link-id))))
    (when (and (not (eq type 'link)) org-roam-peek--shown)
      (progn (posframe-hide org-roam-peek-buffer-name)
             (setq org-roam-peek--shown nil)))))

(defun org-roam-peek--posframe-size (num-or-func)
  "Get the height or width. NUM-OR-FUNC is either a number or function."
  (cond ((functionp num-or-func) (funcall num-or-func))
	((numberp num-or-func) num-or-func)
	(t (error "Must be function or number."))))

(defun org-roam-peek--show-posframe (buffer)
  "Show a posframe with the peeked BUFFER."
  (apply #'posframe-show buffer
         :position (point)
         :poshandler org-roam-peek-posframe-poshandler
         :border-width org-roam-peek-posframe-border-width
         :border-color
         (face-attribute 'org-roam-peek-posframe-border :background nil t)
         (list :width
	       (org-roam-peek--posframe-size org-roam-peek-posframe-width)
	       :height
	       (org-roam-peek--posframe-size org-roam-peek-posframe-height))))

;;;###autoload
(define-minor-mode org-roam-peek-mode
  "Toggle org-roam link peeking."
  :lighter " pk"
  :global nil
  (if org-roam-peek--enabled
      (progn (remove-hook 'post-command-hook 'org-roam-peek--post-cmd)
	     (kill-buffer org-roam-peek-buffer-name)
	     (setq org-roam-peek--shown nil
		   org-roam-peek--enabled nil
		   org-roam-peek--last-id nil))
    (when (posframe-workable-p)
      (setq org-roam-peek--enabled t)
      (add-hook 'post-command-hook 'org-roam-peek--post-cmd))))

(provide 'org-roam-peek)

;;; org-roam-peek.el ends here.

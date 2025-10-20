;;; exwm-mode-line.el --- EXWM mode-line support -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Free Software Foundation, Inc.

;; Author: Steven Allen <steven@stebalien.com>

;; This file is part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; This module adds support for customizing the mode-line of EXWM
;; buffers.

;; At the moment, it contains one mode
;; (`exwm-mode-line-icon-mode') that adds the X window's icon to the
;; mode-line. To do this, enable this mode as follows:
;;
;;   (exwm-mode-line-icon-mode 1)

;;; Code:

(require 'exwm-core)

(declare-function exwm-icon "exwm.el" (&optional id &rest props))

(defgroup exwm-mode-line nil
  "EXWM mode line customizations."
  :group 'exwm)

(defcustom exwm-mode-line-icon-position 'before-buffer-name
  "Position of the X window icon on the mode-line."
  :group 'exwm-mode-line
  :type '(choice (const :tag "None (user must place the mode-line segment)" nil)
                 (const :tag "Before the buffer name" before-buffer-name)
                 (const :tag "After the buffer name" after-buffer-name))
  :set
  (lambda (symbol value)
    (custom-set-default symbol value)
    (when exwm-mode-line-icon-mode
      (exwm-mode-line--place-icon)
      (force-mode-line-update t))))

(defcustom exwm-mode-line-icon-height 1.0
  "Height of the EXWM mode-line icon.
The height is a fraction of the mode-line font height."
  :group 'exwm-mode-line
  :type 'float)

(defvar-local exwm-mode-line--icon nil
"The EXWM mode line icon.")
(put 'exwm-mode-line--icon 'risky-local-variable t)

(defun exwm-mode-line--setup ()
  "Setup the EXWM mode-line in the current buffer."
  (exwm-mode-line--update-icon))

(defun exwm-mode-line--update-icon ()
  "Update the X window icon in the current buffer."
  (when-let* ((icon (exwm-icon nil
                               :ascent 'center
                               :scale 1.0
                               :height `(,exwm-mode-line-icon-height . ch))))
    (setq exwm-mode-line--icon
          (concat " " (propertize " " 'display icon) " "))
    (force-mode-line-update)))

(defun exwm-mode-line--place-icon ()
  (when exwm-mode-line-icon-position
    (cl-callf2 remove
        '(exwm-mode-line--icon exwm-mode-line--icon)
        (default-value 'mode-line-buffer-identification))
    (pcase-exhaustive exwm-mode-line-icon-position
      ('before-buffer-name
       (push '(exwm-mode-line--icon exwm-mode-line--icon)
             (default-value 'mode-line-buffer-identification)))
      ('after-buffer-name
       (cl-callf append (default-value 'mode-line-buffer-identification)
         '((exwm-mode-line--icon exwm-mode-line--icon)))))))

;;;###autoload
(define-minor-mode exwm-mode-line-icon-mode
  "Display X window icons in the mode-line."
  :global t
  :group 'exwm-mode-line
  (if exwm-mode-line-icon-mode
      (progn
        (add-hook 'exwm-update-icon-hook #'exwm-mode-line--update-icon)
        (add-hook 'exwm-mode-hook #'exwm-mode-line--setup)
        (dolist (pair exwm--id-buffer-alist)
          (with-current-buffer (cdr pair)
            (exwm-mode-line--setup))))
    (remove-hook 'exwm-update-icon-hook #'exwm-mode-line--update-icon)
    (remove-hook 'exwm-mode-hook #'exwm-mode-line--setup)
    (dolist (pair exwm--id-buffer-alist)
      (with-current-buffer (cdr pair)
        (setq exwm-mode-line--icon nil)))
    (force-mode-line-update t)))

(provide 'exwm-mode-line)
;;; exwm-mode-line.el ends here

;;; exwm-xsettings.el --- XSETTINGS Module for EXWM -*- lexical-binding: t -*-

;; Copyright (C) 2022-2025 Free Software Foundation, Inc.

;; Author: Steven Allen <steven@stebalien.com>

;; This file is part of GNU Emacs.

;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Implements the XSETTINGS protocol, allowing Emacs to manage the system theme,
;; fonts, icons, etc.
;;
;; This package can be configured as follows:
;;
;;   (setq exwm-xsettings-theme '("Adwaita" . "Adwaita-dark") ;; light/dark
;;         exwm-xsettings `(("Xft/HintStyle" . "hintslight")
;;                          ("Xft/RGBA" . "rgb")
;;                          ("Xft/lcdfilter" . "lcddefault")
;;                          ("Xft/Antialias" . 1)
;;                          ;; DPI is in 1024ths of an inch, so this is a DPI of
;;                          ;; 144, equivalent to ;; a scaling factor of 1.5
;;                          ;; (144 = 1.5 * 96).
;;                          ("Xft/DPI" . ,(* 144 1024))
;;                          ("Xft/Hinting" . 1)))
;;   (exwm-xsettings-mode 1)
;;
;; To modify these settings at runtime, customize them with
;; `custom-set-variables' or `setopt' (Emacs 29+).  E.g., the following will
;; immediately change the icon theme to "Papirus" at runtime, even in running
;; applications:
;;
;;   (setopt exwm-xsettings-icon-theme "Papirus")

;;; Code:

(require 'xcb-ewmh)
(require 'xcb-xsettings)
(require 'exwm-core)

(defgroup exwm-xsettings nil
  "XSETTINGS."
  :group 'exwm)

(defvar exwm-xsettings--connection nil)
(defvar exwm-xsettings--XSETTINGS_SETTINGS-atom nil)
(defvar exwm-xsettings--XSETTINGS_S0-atom nil)
(defvar exwm-xsettings--selection-owner-window nil)
(defvar exwm-xsettings--serial 0)

;;;###autoload
(define-minor-mode exwm-xsettings-mode
  "Toggle EXWM xsettings support."
  :global t
  :group 'exwm-xsettings
  (exwm--global-minor-mode-body xsettings))

(defun exwm-xsettings--rgba-match (_widget value)
  "Return t if VALUE is a valid RGBA color."
  (and (numberp value) (<= 0 value 1)))

(defun exwm-xsettings--custom-set (symbol value)
  "Setter used by `exwm-xsettings' customization options.

SYMBOL is the setting being updated and VALUE is the new value."
  (set-default-toplevel-value symbol value)
  (when exwm-xsettings-mode (exwm-xsettings--update-settings)))

(defcustom exwm-xsettings nil
  "Alist of custom XSETTINGS.
These settings take precedence over `exwm-xsettings-theme' and
`exwm-xsettings-icon-theme'."
  :type '(alist :key-type (string :tag "Name")
                :value-type (choice :tag "Value"
                              (string :tag "String")
                              (integer :tag "Integer")
                              (list :tag "Color"
                                (number :tag "Red"
                                        :type-error
                                        "This field should contain a number between 0 and 1."
                                       :match exwm-xsettings--rgba-match)
                                (number :tag "Green"
                                        :type-error
                                        "This field should contain a number between 0 and 1."
                                       :match exwm-xsettings--rgba-match)
                                (number :tag "Blue"
                                        :type-error
                                        "This field should contain a number between 0 and 1."
                                       :match exwm-xsettings--rgba-match)
                                (number :tag "Alpha"
                                        :type-error
                                        "This field should contain a number between 0 and 1."
                                       :match exwm-xsettings--rgba-match
                                       :value 1.0))))
  :initialize #'custom-initialize-default
  :set #'exwm-xsettings--custom-set)

(defcustom exwm-xsettings-theme nil
  "The system-wide theme."
  :type '(choice (string :tag "Theme")
                 (cons (string :tag "Light Theme")
                       (string :tag "Dark Theme")))
  :initialize #'custom-initialize-default
  :set #'exwm-xsettings--custom-set)

(defcustom exwm-xsettings-icon-theme nil
  "The system-wide icon theme."
  :type '(choice (string :tag "Icon Theme")
                 (cons (string :tag "Light Icon Theme")
                       (string :tag "Dark Icon Theme")))
  :initialize #'custom-initialize-default
  :set #'exwm-xsettings--custom-set)

(defun exwm-xsettings--pick-theme (theme)
  "Pick a light or dark theme from the given THEME.
If THEME is a string, it's returned directly.
If THEME is a cons of (LIGHT . DARK), the appropriate theme is picked based on
the default face's background color."
  (pcase theme
    ((cl-type string) theme)
    (`(,(cl-type string) . ,(cl-type string))
     (if (color-dark-p (color-name-to-rgb (face-background 'default)))
         (cdr theme) (car theme)))
    (_ (error "Expected theme to be a string or a pair of strings"))))

(defun exwm-xsettings--get-settings ()
  "Get the current settings.
Combines `exwm-xsettings', `exwm-xsettings-theme' (if set), and
`exwm-xsettings-icon-theme' (if set)."
  (cl-remove-duplicates
   (append
    exwm-xsettings
    (when exwm-xsettings-theme
      (list (cons "Net/ThemeName" (exwm-xsettings--pick-theme exwm-xsettings-theme))))
    (when exwm-xsettings-icon-theme
      (list (cons "Net/IconThemeName" (exwm-xsettings--pick-theme exwm-xsettings-icon-theme)))))
   :key 'car
   :test 'string=))

(defun exwm-xsettings--make-settings (settings serial)
  "Construct a new settings object.
SETTINGS is an alist of key/value pairs.
SERIAL is a sequence number."
  (make-instance 'xcb:xsettings:-Settings
                 :byte-order (if xcb:lsb 0 1)
                 :serial serial
                 :settings-len (length settings)
                 :settings
                 (mapcar
                  (lambda (prop)
                    (let* ((name (car prop))
                           (value (cdr prop))
                           (common (list :name name
                                         :name-len (length name)
                                         :last-change-serial serial)))
                      (pcase value
                        ((cl-type string)
                         (apply #'make-instance 'xcb:xsettings:-SETTING_STRING
                                :value-len (length value)
                                :value value
                                common))
                        ((cl-type integer)
                         (apply #'make-instance 'xcb:xsettings:-SETTING_INTEGER
                                :value value common))
                        ((and (cl-type list) (app length (or 3 4)))
                         ;; Convert from RGB(A) to 16bit integers.
                         (setq value (mapcar (lambda (x) (round (* x #xffff))) value))
                         (apply #'make-instance 'xcb:xsettings:-SETTING_COLOR
                                :red (pop value)
                                :green (pop value)
                                :blue (pop value)
                                :alpha (or (pop value) #xffff)))
                        (_ (error "Setting value must be a string, integer, or length 3-4 list")))))
                  settings)))

(defun exwm-xsettings--update-settings ()
  "Update the xsettings."
  (when exwm-xsettings--connection
    (setq exwm-xsettings--serial (1+ exwm-xsettings--serial))
    (let* ((settings (exwm-xsettings--get-settings))
           (bytes (xcb:marshal (exwm-xsettings--make-settings settings exwm-xsettings--serial))))
      (xcb:+request exwm-xsettings--connection
          (make-instance 'xcb:ChangeProperty
                         :mode xcb:PropMode:Replace
                         :window exwm-xsettings--selection-owner-window
                         :property exwm-xsettings--XSETTINGS_SETTINGS-atom
                         :type exwm-xsettings--XSETTINGS_SETTINGS-atom
                         :format 8
                         :data-len (length bytes)
                         :data bytes)))
    (xcb:flush exwm-xsettings--connection)))

(defun exwm-xsettings--on-theme-change (&rest _)
  "Called when the Emacs theme is changed."
  ;; We only bother updating the xsettings if changing the theme could effect
  ;; the settings.
  (when (or (consp exwm-xsettings-theme) (consp exwm-xsettings-icon-theme))
    (exwm-xsettings--update-settings)))

(defun exwm-xsettings--on-SelectionClear (_data _synthetic)
  "Called when another xsettings daemon takes over."
  (exwm--log "XSETTINGS manager has been replaced.")
  (exwm-xsettings--exit))

(cl-defun exwm-xsettings--init ()
  "Initialize the XSETTINGS module."
  (exwm--log)

  ;; idempotent initialization
  (when exwm-xsettings--connection
    (cl-return-from exwm-xsettings--init))

  ;; Connect
  (setq exwm-xsettings--connection (xcb:connect))
  (set-process-query-on-exit-flag (slot-value exwm-xsettings--connection
                                              'process)
                                  nil)

  ;; Intern the atoms.
  (setq exwm-xsettings--XSETTINGS_SETTINGS-atom
        (exwm--intern-atom "_XSETTINGS_SETTINGS" exwm-xsettings--connection)

        exwm-xsettings--XSETTINGS_S0-atom
        (exwm--intern-atom "_XSETTINGS_S0" exwm-xsettings--connection))

  ;; Detect running XSETTINGS managers.
  (with-slots (owner)
      (xcb:+request-unchecked+reply exwm-xsettings--connection
          (make-instance 'xcb:GetSelectionOwner
                         :selection exwm-xsettings--XSETTINGS_S0-atom))
    (when (/= owner xcb:Window:None)
      (xcb:disconnect exwm-xsettings--connection)
      (setq exwm-xsettings--connection nil)
      (warn "[EXWM] Other XSETTINGS manager detected")
      (cl-return-from exwm-xsettings--init)))

  (let ((id(xcb:generate-id exwm-xsettings--connection)))
    (setq exwm-xsettings--selection-owner-window id)

    ;; Create a settings window.
    (xcb:+request exwm-xsettings--connection
        (make-instance 'xcb:CreateWindow
                       :wid id
                       :parent exwm--root
                       :class xcb:WindowClass:InputOnly
                       :x 0
                       :y 0
                       :width 1
                       :height 1
                       :border-width 0
                       :depth 0
                       :visual 0
                       :value-mask xcb:CW:OverrideRedirect
                       :override-redirect 1))

    ;; Set _NET_WM_NAME.
    (xcb:+request exwm-xsettings--connection
        (make-instance 'xcb:ewmh:set-_NET_WM_NAME
                       :window id
                       :data "EXWM: exwm-xsettings--selection-owner-window"))

    ;; Apply the XSETTINGS properties.
    (exwm-xsettings--update-settings)

    ;; Take ownership and notify.
    (xcb:+request exwm-xsettings--connection
        (make-instance 'xcb:SetSelectionOwner
                       :owner id
                       :selection exwm-xsettings--XSETTINGS_S0-atom
                       :time xcb:Time:CurrentTime))
    (xcb:+request exwm-xsettings--connection
        (make-instance 'xcb:SendEvent
                       :propagate 0
                       :destination exwm--root
                       :event-mask xcb:EventMask:StructureNotify
                       :event (xcb:marshal
                               (make-instance 'xcb:icccm:-ManagerSelection
                                              :window exwm--root
                                              :time xcb:Time:CurrentTime
                                              :selection exwm-xsettings--XSETTINGS_S0-atom
                                              :owner id)
                               exwm-xsettings--connection)))

    ;; Detect loss of XSETTINGS ownership.
    (xcb:+event exwm-xsettings--connection 'xcb:SelectionClear
                #'exwm-xsettings--on-SelectionClear)

    (xcb:flush exwm-xsettings--connection))

  ;; Update the xsettings if/when the theme changes.
  (add-hook 'enable-theme-functions #'exwm-xsettings--on-theme-change)
  (add-hook 'disable-theme-functions #'exwm-xsettings--on-theme-change))

(defun exwm-xsettings--exit ()
  "Exit the XSETTINGS module."
  (exwm--log)

  (when exwm-xsettings--connection
    (remove-hook 'enable-theme-functions #'exwm-xsettings--on-theme-change)
    (remove-hook 'disable-theme-functions #'exwm-xsettings--on-theme-change)

    (xcb:disconnect exwm-xsettings--connection)

    (setq exwm-xsettings--connection nil
          exwm-xsettings--XSETTINGS_SETTINGS-atom nil
          exwm-xsettings--XSETTINGS_S0-atom nil
          exwm-xsettings--selection-owner-window nil)))

(provide 'exwm-xsettings)
;;; exwm-xsettings.el ends here

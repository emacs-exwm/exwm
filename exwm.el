;;; exwm.el --- Emacs X Window Manager  -*- lexical-binding: t -*-

;; Copyright (C) 2015-2026 Free Software Foundation, Inc.

;; Author: Chris Feng <chris.w.feng@gmail.com>
;; Maintainer: Adrián Medraño Calvo <adrian@medranocalvo.com>, Steven Allen <steven@stebalien.com>, Daniel Mendler <mail@daniel-mendler.de>
;; Version: 0.34
;; Package-Requires: ((emacs "27.1") (xelb "0.22") (compat "30"))
;; Keywords: unix
;; URL: https://github.com/emacs-exwm/exwm

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

;; Overview
;; --------
;; EXWM (Emacs X Window Manager) is a full-featured tiling X window manager
;; for Emacs built on top of [XELB](https://github.com/emacs-exwm/xelb).
;; It features:
;; + Fully keyboard-driven operations
;; + Hybrid layout modes (tiling & stacking)
;; + Dynamic workspace support
;; + ICCCM/EWMH compliance
;; Optional features:
;; + RandR (multi-monitor) support
;; + System tray
;; + Input method
;; + Background setting support
;; + XSETTINGS server

;; Installation & configuration
;; ----------------------------
;; Here are the minimal steps to get EXWM working:
;; 1. Install XELB and EXWM, and make sure they are in `load-path'.
;; 2. In '~/.emacs', add following lines (please modify accordingly):
;;
;;    (require 'exwm)
;;    (setq exwm-input-global-keys `(([?\s-r] . exwm-reset)))
;;    (exwm-wm-mode)
;;
;; 3. Add the following lines to '~/.xinitrc':
;;
;;    exec emacs
;;
;; 4. Launch EXWM in a console (e.g. tty1) with
;;
;;    xinit -- vt01
;;
;; You should additionally hide the menu-bar, tool-bar, etc to increase the
;; usable space.  Please check the wiki (https://github.com/emacs-exwm/exwm/wiki)
;; for more detailed instructions on installation, configuration, usage, etc.

;; References:
;; + dwm (https://dwm.suckless.org/)
;; + i3 wm (https://i3wm.org/)
;; + Also see references within each required library.

;;; Code:

(require 'server)
(require 'exwm-core)
(require 'exwm-workspace)
(require 'exwm-layout)
(require 'exwm-floating)
(require 'exwm-manage)
(require 'exwm-input)
(eval-when-compile (require 'subr-x)) ;; Needed on 28 for when-let*

(declare-function x-get-atom-name "C source code" (VALUE &optional FRAME))

(defgroup exwm nil
  "Emacs X Window Manager."
  :tag "EXWM"
  :group 'applications
  :prefix "exwm-")

(defcustom exwm-init-hook nil
  "Normal hook run when EXWM has just finished initialization."
  :type 'hook)

(defcustom exwm-exit-hook nil
  "Normal hook run just before EXWM exits."
  :type 'hook)

(defcustom exwm-update-class-hook nil
  "Normal hook run when window class is updated."
  :type 'hook)

(defcustom exwm-update-title-hook nil
  "Normal hook run when window title is updated."
  :type 'hook)

(defcustom exwm-update-icon-hook nil
  "Normal hook run when window icon is updated."
  :type 'hook)

(defcustom exwm-blocking-subrs
  ;; `x-file-dialog' and `x-select-font' are missing on some Emacs builds, for
  ;; example on the X11 Lucid build.
  '(x-file-dialog x-popup-dialog x-select-font message-box message-or-box)
  "Subrs (primitives) that would normally block EXWM."
  :type '(repeat function))

(defcustom exwm-replace 'ask
  "Whether to replace existing window manager."
  :type '(radio (const :tag "Ask" ask)
                (const :tag "Replace by default" t)
                (const :tag "Do not replace" nil)))

(defconst exwm--server-name "server-exwm"
  "Name of the subordinate Emacs server.")

(defvar exwm--server-timeout 1
  "Number of seconds to wait for the subordinate Emacs server to exit.
After this time, the server will be killed.")

(defvar exwm--server-process nil "Process of the subordinate Emacs server.")

(defvar exwm--client-message-functions nil
  "Alist of form ((MESSAGE . MESSAGE-HANDLER)...).
Set during `exwm--init'.")

(defun exwm-reset ()
  "Reset the state of the selected window (non-fullscreen, line-mode, etc)."
  (interactive)
  (exwm--log)
  (with-current-buffer (window-buffer)
    (when (derived-mode-p 'exwm-mode)
      (when (exwm-layout--fullscreen-p)
        (exwm-layout-unset-fullscreen))
      ;; Force refresh
      (exwm-layout--refresh)
      (call-interactively #'exwm-input-grab-keyboard))))

(defun exwm-restart ()
  "Restart EXWM."
  (declare (obsolete restart-emacs "0.34"))
  (interactive)
  (message "Use `restart-emacs' instead of the obsolete `exwm-restart'.")
  (exwm--log)
  (when (exwm--confirm-kill-emacs "Restart" 'no-check)
    (let* ((attr (process-attributes (emacs-pid)))
           (args (cdr (assq 'args attr)))
           (ppid (cdr (assq 'ppid attr)))
           (pargs (cdr (assq 'args (process-attributes ppid)))))
      (cond
       ((= ppid 1)
        ;; The parent is the init process.  This probably means this
        ;; instance is an emacsclient.  Anyway, start a control instance
        ;; to manage the subsequent ones.
        (call-process (car command-line-args))
        (kill-emacs))
       ((string= args pargs)
        ;; This is a subordinate instance.  Return a magic number to
        ;; inform the parent (control instance) to start another one.
        (kill-emacs ?R))
       (t
        ;; This is the control instance.  Keep starting subordinate
        ;; instances until told to exit.
        ;; Run `server-force-stop' if it exists.
        (run-hooks 'kill-emacs-hook)
        (with-temp-buffer
          (while (= ?R (shell-command-on-region (point) (point) args))))
        (kill-emacs))))))

(defun exwm--update-desktop (xwin)
  "Update _NET_WM_DESKTOP.
Argument XWIN contains the X window of the `exwm-mode' buffer."
  (exwm--log "#x%x" xwin)
  (with-slots (value)
      (xcb:+request-unchecked+reply exwm--connection
          (make-instance 'xcb:ewmh:get-_NET_WM_DESKTOP
                         :window xwin))
    (exwm--set-desktop xwin value)))

(defun exwm--set-desktop (id desktop)
  "Set ID's workspace to DESKTOP."
  (when-let* ((buffer (exwm--id->buffer id))
              (_(buffer-live-p buffer)))
    (with-current-buffer buffer
      (cond
       ((and desktop (= desktop #xffffffff))
        (unless (or (not exwm--floating-frame)
                    (eq exwm--frame exwm-workspace--current)
                    (and exwm--desktop
                         (= desktop exwm--desktop)))
          (exwm-layout--show id (frame-root-window exwm--floating-frame)))
        (setq exwm--desktop desktop))
       ((and desktop
             (< desktop (exwm-workspace--count))
             (if exwm--desktop
                 (/= desktop exwm--desktop)
               (/= desktop (exwm-workspace--position exwm--frame))))
        (exwm-workspace-move-window desktop id))
       (t
        (exwm-workspace--set-desktop id))))))

(defun exwm--update-window-type (id &optional force)
  "Update `exwm-window-type' from _NET_WM_WINDOW_TYPE.
Argument ID contains the X window of the `exwm-mode' buffer.

When FORCE is nil the update only takes place if
`exwm-window-type' is unset."
  (exwm--log "#x%x" id)
  (with-current-buffer (exwm--id->buffer id)
    (unless (and exwm-window-type (not force))
      (let ((reply (xcb:+request-unchecked+reply exwm--connection
                       (make-instance 'xcb:ewmh:get-_NET_WM_WINDOW_TYPE
                                      :window id))))
        (when reply                     ;nil when destroyed
          (setq exwm-window-type (append (slot-value reply 'value) nil)))))))

(defun exwm--update-class (id &optional force)
  "Update `exwm-instance-name' and `exwm-class' from WM_CLASS.
Argument ID contains the X window of the `exwm-mode' buffer.

When FORCE is nil the update only takes place if any of
`exwm-instance-name' or `exwm-class' is unset."
  (exwm--log "#x%x" id)
  (with-current-buffer (exwm--id->buffer id)
    (unless (and exwm-instance-name exwm-class-name (not force))
      (let ((reply (xcb:+request-unchecked+reply exwm--connection
                       (make-instance 'xcb:icccm:get-WM_CLASS :window id))))
        (when reply                     ;nil when destroyed
          (setq exwm-instance-name (slot-value reply 'instance-name)
                exwm-class-name (slot-value reply 'class-name))
          (when (and exwm-instance-name exwm-class-name)
            (run-hooks 'exwm-update-class-hook)))))))

(defun exwm--update-utf8-title (id &optional force)
  "Update `exwm-title' from _NET_WM_NAME.
Argument ID contains the X window of the `exwm-mode' buffer.

When FORCE is nil the update only takes place if `exwm-title' is
unset."
  (exwm--log "#x%x" id)
  (with-current-buffer (exwm--id->buffer id)
    (when (or force (not exwm-title))
      (let ((reply (xcb:+request-unchecked+reply exwm--connection
                       (make-instance 'xcb:ewmh:get-_NET_WM_NAME :window id))))
        (when reply                     ;nil when destroyed
          (setq exwm-title (slot-value reply 'value))
          (when exwm-title
            (setq exwm--title-is-utf8 t)
            (run-hooks 'exwm-update-title-hook)))))))

(defun exwm--update-ctext-title (id &optional force)
  "Update `exwm-title' from WM_NAME.
Argument ID contains the X window of the `exwm-mode' buffer.

When FORCE is nil the update only takes place if `exwm-title' is
unset."
  (exwm--log "#x%x" id)
  (with-current-buffer (exwm--id->buffer id)
    (unless (or exwm--title-is-utf8
                (and exwm-title (not force)))
      (let ((reply (xcb:+request-unchecked+reply exwm--connection
                       (make-instance 'xcb:icccm:get-WM_NAME :window id))))
        (when reply                     ;nil when destroyed
          (setq exwm-title (slot-value reply 'value))
          (when exwm-title
            (run-hooks 'exwm-update-title-hook)))))))

(defun exwm--update-title (id)
  "Update _NET_WM_NAME or WM_NAME.
Argument ID contains the X window of the `exwm-mode' buffer."
  (exwm--log "#x%x" id)
  (exwm--update-utf8-title id)
  (exwm--update-ctext-title id))

(defun exwm--update-transient-for (id &optional force)
  "Update `exwm-transient-for' from WM_TRANSIENT_FOR.
Argument ID contains the X window of the `exwm-mode' buffer.

When FORCE is nil the update only takes place if `exwm-title' is
unset."
  (exwm--log "#x%x" id)
  (with-current-buffer (exwm--id->buffer id)
    (unless (and exwm-transient-for (not force))
      (let ((reply (xcb:+request-unchecked+reply exwm--connection
                       (make-instance 'xcb:icccm:get-WM_TRANSIENT_FOR
                                      :window id))))
        (when reply                     ;nil when destroyed
          (setq exwm-transient-for (slot-value reply 'value)))))))

(defun exwm--update-normal-hints (id &optional force)
  "Update normal hints from WM_NORMAL_HINTS.
Argument ID contains the X window of the `exwm-mode' buffer.

When FORCE is nil the update only takes place all of
`exwm--normal-hints-x exwm--normal-hints-y',
`exwm--normal-hints-width exwm--normal-hints-height',
`exwm--normal-hints-min-width exwm--normal-hints-min-height' and
`exwm--normal-hints-max-width exwm--normal-hints-max-height' are
unset."
  (exwm--log "#x%x" id)
  (with-current-buffer (exwm--id->buffer id)
    (unless (and (not force)
                 (or exwm--normal-hints-x exwm--normal-hints-y
                     exwm--normal-hints-width exwm--normal-hints-height
                     exwm--normal-hints-min-width exwm--normal-hints-min-height
                     exwm--normal-hints-max-width exwm--normal-hints-max-height
                     ;; FIXME: other fields
                     ))
      (let ((reply (xcb:+request-unchecked+reply exwm--connection
                       (make-instance 'xcb:icccm:get-WM_NORMAL_HINTS
                                      :window id))))
        (when (and reply (slot-value reply 'flags)) ;nil when destroyed
          (with-slots (flags x y width height min-width min-height max-width
                             max-height base-width base-height ;; win-gravity
                             )
              reply
            (unless (= 0 (logand flags xcb:icccm:WM_SIZE_HINTS:USPosition))
              (setq exwm--normal-hints-x x exwm--normal-hints-y y))
            (unless (= 0 (logand flags xcb:icccm:WM_SIZE_HINTS:USSize))
              (setq exwm--normal-hints-width width
                    exwm--normal-hints-height height))
            (unless (= 0 (logand flags xcb:icccm:WM_SIZE_HINTS:PMinSize))
              (setq exwm--normal-hints-min-width min-width
                    exwm--normal-hints-min-height min-height))
            (unless (= 0 (logand flags xcb:icccm:WM_SIZE_HINTS:PMaxSize))
              (setq exwm--normal-hints-max-width max-width
                    exwm--normal-hints-max-height max-height))
            (unless (or exwm--normal-hints-min-width
                        (= 0 (logand flags xcb:icccm:WM_SIZE_HINTS:PBaseSize)))
              (setq exwm--normal-hints-min-width base-width
                    exwm--normal-hints-min-height base-height))
            ;; (unless (= 0 (logand flags xcb:icccm:WM_SIZE_HINTS:PWinGravity))
            ;;   (setq exwm--normal-hints-win-gravity win-gravity))
            (setq exwm--fixed-size
                  (and exwm--normal-hints-min-width
                       exwm--normal-hints-min-height
                       exwm--normal-hints-max-width
                       exwm--normal-hints-max-height
                       (/= 0 exwm--normal-hints-min-width)
                       (/= 0 exwm--normal-hints-min-height)
                       (= exwm--normal-hints-min-width
                          exwm--normal-hints-max-width)
                       (= exwm--normal-hints-min-height
                          exwm--normal-hints-max-height)))))))))

(defun exwm--update-hints (id &optional force)
  "Update hints from WM_HINTS.
Argument ID contains the X window of the `exwm-mode' buffer.

When FORCE is nil the update only takes place if both of
`exwm--hints-input' and `exwm--hints-urgency' are unset."
  (exwm--log "#x%x" id)
  (with-current-buffer (exwm--id->buffer id)
    (unless (and (not force) exwm--hints-input exwm--hints-urgency)
      (let ((reply (xcb:+request-unchecked+reply exwm--connection
                       (make-instance 'xcb:icccm:get-WM_HINTS :window id))))
        (when (and reply (slot-value reply 'flags)) ;nil when destroyed
          (with-slots (flags input initial-state) reply
            (when flags
              (unless (= 0 (logand flags xcb:icccm:WM_HINTS:InputHint))
                (setq exwm--hints-input (when input (= 1 input))))
              (unless (= 0 (logand flags xcb:icccm:WM_HINTS:StateHint))
                (setq exwm-state initial-state))
              (unless (= 0 (logand flags xcb:icccm:WM_HINTS:UrgencyHint))
                (setq exwm--hints-urgency t))))
          (when (and exwm--hints-urgency
                     (not (eq exwm--frame exwm-workspace--current)))
            (unless (frame-parameter exwm--frame 'exwm-urgency)
              (set-frame-parameter exwm--frame 'exwm-urgency t)
              (setq exwm-workspace--switch-history-outdated t))))))))

(defun exwm--update-protocols (id &optional force)
  "Update `exwm--protocols' from WM_PROTOCOLS.
Argument ID contains the X window of the `exwm-mode' buffer.

When FORCE is nil the update only takes place if `exwm--protocols'
is unset."
  (exwm--log "#x%x" id)
  (with-current-buffer (exwm--id->buffer id)
    (unless (and exwm--protocols (not force))
      (let ((reply (xcb:+request-unchecked+reply exwm--connection
                       (make-instance 'xcb:icccm:get-WM_PROTOCOLS
                                      :window id))))
        (when reply                     ;nil when destroyed
          (setq exwm--protocols (append (slot-value reply 'value) nil)))))))

(defun exwm--update-struts-legacy (id)
  "Update struts of X window ID from _NET_WM_STRUT."
  (exwm--log "#x%x" id)
  (let ((pair (assq id exwm-workspace--id-struts-alist))
        reply struts)
    (unless (and pair (< 4 (length (cdr pair))))
      (setq reply (xcb:+request-unchecked+reply exwm--connection
                      (make-instance 'xcb:ewmh:get-_NET_WM_STRUT
                                     :window id)))
      (when reply
        (setq struts (slot-value reply 'value))
        (if pair
            (setcdr pair struts)
          (push (cons id struts) exwm-workspace--id-struts-alist))
        (exwm-workspace--update-struts))
      ;; Update workareas.
      (exwm-workspace--update-workareas)
      ;; Update workspaces.
      (dolist (f exwm-workspace--list)
        (exwm-workspace--set-fullscreen f)))))

(defun exwm--update-struts-partial (id)
  "Update struts of X window ID from _NET_WM_STRUT_PARTIAL."
  (exwm--log "#x%x" id)
  (let ((reply (xcb:+request-unchecked+reply exwm--connection
                   (make-instance 'xcb:ewmh:get-_NET_WM_STRUT_PARTIAL
                                  :window id)))
        struts pair)
    (when reply
      (setq struts (slot-value reply 'value)
            pair (assq id exwm-workspace--id-struts-alist))
      (if pair
          (setcdr pair struts)
        (push (cons id struts) exwm-workspace--id-struts-alist))
      (exwm-workspace--update-struts))
    ;; Update workareas.
    (exwm-workspace--update-workareas)
    ;; Update workspaces.
    (dolist (f exwm-workspace--list)
      (exwm-workspace--set-fullscreen f))))

(defun exwm--update-struts (id)
  "Update struts of X window ID from _NET_WM_STRUT_PARTIAL or _NET_WM_STRUT."
  (exwm--log "#x%x" id)
  (exwm--update-struts-partial id)
  (exwm--update-struts-legacy id))

(defsubst exwm--icon-index-colors (data offset length)
  "Index the colors in an ARGB vector.
DATA is the ARGB vector where OFFSET and LENGTH are the bounds of the
image.

Return (COLORS . PIXELS) where COLORS is the color index (vector of colors)
and PIXELS is the input image with the raw ARGB values replaced by their
indices in COLORS."
  (let* ((colorhash (make-hash-table :size length))
         (pixels (make-vector length 0)))
    (dotimes (i length)
      (let ((pixel (aref data (+ i offset))))
        (if (= 0 (logand #xFF000000 pixel))
            (aset pixels i 0)
          (aset pixels i
                (with-memoization
                    ;; Alpha values other than "fully transparent"
                    ;; are ignored.
                    (gethash (logand pixel #xFFFFFF) colorhash)
                  (1+ (hash-table-count colorhash)))))))
    (let ((colors (make-vector (1+ (hash-table-count colorhash)) nil)))
      (aset colors 0 "None")
      (maphash
       (lambda (color idx) (aset colors idx (format "#%06X" color)))
       colorhash)
      (cons colors pixels))))

(defsubst exwm--icon-colornames (count)
  "Return a vector of unique color names (strings) for COUNT colors."
  (let* ((colorlen (length (format "%x" count)))
         (vec (make-vector (1+ count) nil))
         (fmt (format "%%0%dx" colorlen)))
    (dotimes (i (length vec))
      (aset vec i (format fmt i)))
    vec))

(defun exwm--icon-to-xpm (data)
  "Convert the ARGB image vector DATA into an XPM image.
DATA must be a vector the form [width, height, pixels...].
If DATA contains multiple images, only the first is used."
  (pcase-let* ((width (aref data 0))
               (height (aref data 1))
               ;; RGB -> color index & pixel vector.
               (`(,colors . ,pixels)
                (exwm--icon-index-colors data 2 (* width height)))
               ;; index -> color string mapping (for performance)
               (colornames (exwm--icon-colornames (length colors)))
               ;; We collect our output into a "chunks" list and
               ;; reverse/concat at the end for better performance
               ;; than inserting into a temporary buffer.
               (chunks nil))
    ;; Insert the XPM header.
    (push
     (format "/* XPM */\nstatic char * XFACE[] = {\n\"%d %d %d %d\",\n"
             width height
             (length colors)
             (length (aref colornames 0)))
          chunks)
    ;; Insert the color index.
    (dotimes (i (length colors))
      (push "\"" chunks)
      (push (aref colornames i) chunks)
      (push " c " chunks)
      (push (aref colors i) chunks)
      (push "\",\n" chunks))
    ;; Insert the pixels.
    (dotimes (i (length pixels))
      (when (= (% i width) 0) (push "\"" chunks))
      (push (aref colornames (aref pixels i)) chunks)
      (when (= (% (1+ i) width) 0) (push "\",\n" chunks)))
    ;; Insert the XPM footer.
    (push "};\n" chunks)
    ;; And done.
    (apply #'concat (nreverse chunks))))

(defun exwm--update-icon (id &optional force)
  "Update the `exwm--icon' for the X window ID from _NET_WM_ICON.
If FORCE is t, update the icon even if `exwm--icon' is already set."
  (exwm--log "#x%x" id)
  (with-current-buffer (exwm--id->buffer id)
    (when-let* (((or force (not exwm--icon)))
                (reply (xcb:+request-unchecked+reply exwm--connection
                           (make-instance 'xcb:ewmh:-get-_NET_WM_ICON
                                          :window id)))
                (data (slot-value reply 'value))
                ((length> data 0)))
      (condition-case err
          (progn
            (setq exwm--icon (exwm--icon-to-xpm data))
            (when exwm--icon
              (run-hooks 'exwm-update-icon-hook)))
        (warn "[EXWM] Invalid window icon for `%s': %s" exwm-title err)))))

;; LIMITATIONS:
;;
;; - This code currently only processes the first icon, ignoring all
;;   the rest (other sizes). Ideally we'd parse all icons and pick
;;   the best size for what we need, but we'd need to do it lazily
;;   in that case (for performance reasons).
;; - This code doesn't fetch icons by-name (_NET_WM_ICON_NAME). However,
;;   setting this property doesn't seem to be that common anyways.
(defun exwm-icon (&optional id &rest properties)
  "Return the X window icon for the window with ID.

If ID is unspecified or nil, return the icon for the current window.  If
the specified window has no icon, return nil.

PROPERTIES are passed to `create-image'.  See Info node `(elisp)Image
Descriptors' for the list of supported properties."
  (when-let* ((icon (if (and id (/= id exwm--id))
                  (buffer-local-value 'exwm--icon (exwm--id->buffer id))
                exwm--icon)))
    (apply #'create-image icon 'xpm t properties)))

(defun exwm--on-PropertyNotify (data _synthetic)
  "Handle PropertyNotify event.
DATA contains unmarshalled PropertyNotify event data."
  (let* ((obj (xcb:unmarshal-new 'xcb:PropertyNotify data))
         (atom (slot-value obj 'atom))
         (id (slot-value obj 'window))
         (buffer (exwm--id->buffer id)))
    (exwm--log "atom=%s(%s)" (x-get-atom-name atom exwm-workspace--current) atom)
    (if (not (buffer-live-p buffer))
        ;; Properties of unmanaged X windows.
        (cond ((= atom xcb:Atom:_NET_WM_STRUT)
               (exwm--update-struts-legacy id))
              ((= atom xcb:Atom:_NET_WM_STRUT_PARTIAL)
               (exwm--update-struts-partial id)))
      (with-current-buffer buffer
        (cond ((= atom xcb:Atom:_NET_WM_WINDOW_TYPE)
               (exwm--update-window-type id t))
              ((= atom xcb:Atom:WM_CLASS)
               (exwm--update-class id t))
              ((= atom xcb:Atom:_NET_WM_NAME)
               (exwm--update-utf8-title id t))
              ((= atom xcb:Atom:WM_NAME)
               (exwm--update-ctext-title id t))
              ((= atom xcb:Atom:WM_TRANSIENT_FOR)
               (exwm--update-transient-for id t))
              ((= atom xcb:Atom:WM_NORMAL_HINTS)
               (exwm--update-normal-hints id t))
              ((= atom xcb:Atom:WM_HINTS)
               (exwm--update-hints id t))
              ((= atom xcb:Atom:WM_PROTOCOLS)
               (exwm--update-protocols id t))
              ((= atom xcb:Atom:_NET_WM_USER_TIME)) ;ignored
              ((= atom xcb:Atom:_NET_WM_ICON)
               (exwm--update-icon id t))
              (t
               (exwm--log "Unhandled: %s(%d)"
                          (x-get-atom-name atom exwm-workspace--current)
                          atom)))))))

(defun exwm--on-net-number-of-desktops (_id data)
  "Handle _NET_NUMBER_OF_DESKTOPS_ message with DATA."
  (let ((current (exwm-workspace--count))
        (requested (elt data 0)))
    ;; Only allow increasing/decreasing the workspace number by 1.
    (cond
     ((< current requested)
      (make-frame))
     ((and (> current requested)
           (> current 1))
      (let ((frame (car (last exwm-workspace--list))))
        (delete-frame frame))))))

(defun exwm--on-net-current-desktop (_id data)
  "Handle _NET_CURRENT_DESKTOP message with DATA."
  (exwm-workspace-switch (elt data 0)))

(defun exwm--on-net-active-window (id _data)
  "Handle _NET_ACTIVE_WINDOW message with ID."
  (let ((buffer (exwm--id->buffer id))
        window)
    (if (buffer-live-p buffer)
        ;; Either an `exwm-mode' buffer (an X window) or a floating frame.
        (with-current-buffer buffer
          (when (eq exwm--frame exwm-workspace--current)
            (if exwm--floating-frame
                (select-frame exwm--floating-frame)
              (setq window (get-buffer-window nil t))
              (unless window
                ;; State change: iconic => normal.
                (setq window (frame-selected-window exwm--frame))
                (set-window-buffer window (current-buffer)))
              ;; Focus transfer.
              (select-window window))))
      ;; A workspace.
      (dolist (f exwm-workspace--list)
        (when (eq id (frame-parameter f 'exwm-outer-id))
          (x-focus-frame f t))))))

(defun exwm--on-net-close-window (id _data)
  "Handle _NET_CLOSE_WINDOW message with ID."
  (let ((buffer (exwm--id->buffer id)))
    (when (buffer-live-p buffer)
      (exwm--defer 0 #'kill-buffer buffer))))

(defun exwm--on-net-wm-moveresize (id data)
  "Handle _NET_WM_MOVERESIZE message with ID and DATA."
  (let ((direction (elt data 2))
        (buffer (exwm--id->buffer id)))
    (unless (and buffer
                 (not (buffer-local-value 'exwm--floating-frame buffer)))
      (cond ((= direction
                xcb:ewmh:_NET_WM_MOVERESIZE_SIZE_KEYBOARD)
             ;; FIXME
             )
            ((= direction
                xcb:ewmh:_NET_WM_MOVERESIZE_MOVE_KEYBOARD)
             ;; FIXME
             )
            ((= direction xcb:ewmh:_NET_WM_MOVERESIZE_CANCEL)
             (exwm-floating--stop-moveresize))
            ;; In case it's a workspace frame.
            ((and (not buffer)
                  (catch 'break
                    (dolist (f exwm-workspace--list)
                      (when (or (eq id (frame-parameter f 'exwm-outer-id))
                                (eq id (frame-parameter f 'exwm-id)))
                        (throw 'break t)))
                    nil)))
            (t
             ;; In case it's a floating frame,
             ;; move the corresponding X window instead.
             (unless buffer
               (catch 'break
                 (dolist (pair exwm--id-buffer-alist)
                   (with-current-buffer (cdr pair)
                     (when
                         (and exwm--floating-frame
                              (or (eq id
                                      (frame-parameter exwm--floating-frame
                                                       'exwm-outer-id))
                                  (eq id
                                      (frame-parameter exwm--floating-frame
                                                       'exwm-id))))
                       (setq id exwm--id)
                       (throw 'break nil))))))
             ;; Start to move it.
             (exwm-floating--start-moveresize id direction))))))

(defun exwm--on-net-request-frame-extents (id _data)
  "Handle _NET_REQUEST_FRAME_EXTENTS message with ID."
  (let ((buffer (exwm--id->buffer id))
        top btm)
    (if (or (not buffer)
            (not (buffer-local-value 'exwm--floating-frame buffer)))
        (setq top 0
              btm 0)
      (setq top (window-header-line-height)
            btm (window-mode-line-height)))
    (xcb:+request exwm--connection
        (make-instance 'xcb:ewmh:set-_NET_FRAME_EXTENTS
                       :window id
                       :left 0
                       :right 0
                       :top top
                       :bottom btm)))
  (xcb:flush exwm--connection))

(defun exwm--on-net-wm-desktop (id data)
  "Handle _NET_WM_DESKTOP message with ID and DATA."
  (exwm--set-desktop id (elt data 0)))

(defun exwm--on-net-wm-state (id data)
  "Handle _NET_WM_STATE message with ID and DATA."
  (let ((action (elt data 0))
        (props (list (elt data 1) (elt data 2)))
        (buffer (exwm--id->buffer id))
        props-new)
    ;; only support _NET_WM_STATE_FULLSCREEN / _NET_WM_STATE_ADD for frames
    (when (and (not buffer)
               (memq xcb:Atom:_NET_WM_STATE_FULLSCREEN props)
               (= action xcb:ewmh:_NET_WM_STATE_ADD))
      (xcb:+request
          exwm--connection
          (make-instance 'xcb:ewmh:set-_NET_WM_STATE
                         :window id
                         :data (vector xcb:Atom:_NET_WM_STATE_FULLSCREEN)))
      (xcb:flush exwm--connection))
    (when buffer                    ;ensure it's managed
      (with-current-buffer buffer
        ;; _NET_WM_STATE_FULLSCREEN
        (when (or (memq xcb:Atom:_NET_WM_STATE_FULLSCREEN props)
                  (memq xcb:Atom:_NET_WM_STATE_ABOVE props))
          (cond ((= action xcb:ewmh:_NET_WM_STATE_ADD)
                 (unless (exwm-layout--fullscreen-p)
                   (exwm-layout-set-fullscreen id))
                 (push xcb:Atom:_NET_WM_STATE_FULLSCREEN props-new))
                ((= action xcb:ewmh:_NET_WM_STATE_REMOVE)
                 (when (exwm-layout--fullscreen-p)
                   (exwm-layout-unset-fullscreen id)))
                ((= action xcb:ewmh:_NET_WM_STATE_TOGGLE)
                 (if (exwm-layout--fullscreen-p)
                     (exwm-layout-unset-fullscreen id)
                   (exwm-layout-set-fullscreen id)
                   (push xcb:Atom:_NET_WM_STATE_FULLSCREEN props-new)))))
        ;; _NET_WM_STATE_DEMANDS_ATTENTION
        ;; FIXME: check (may require other properties set)
        (when (memq xcb:Atom:_NET_WM_STATE_DEMANDS_ATTENTION props)
          (when (= action xcb:ewmh:_NET_WM_STATE_ADD)
            (unless (eq exwm--frame exwm-workspace--current)
              (set-frame-parameter exwm--frame 'exwm-urgency t)
              (setq exwm-workspace--switch-history-outdated t)))
          ;; xcb:ewmh:_NET_WM_STATE_REMOVE?
          ;; xcb:ewmh:_NET_WM_STATE_TOGGLE?
          )
        (xcb:+request exwm--connection
            (make-instance 'xcb:ewmh:set-_NET_WM_STATE
                           :window id :data (vconcat props-new)))
        (xcb:flush exwm--connection)))))

(defun exwm--on-wm-protocols (id data)
  "Handle WM_PROTOCOLS message with DATA to window ID."
  (let ((type (elt data 0))
        (client (elt data 2)))
    (cond ((= type xcb:Atom:_NET_WM_PING)
             (when-let* (((eq id exwm--root))
                         (buf (exwm--id->buffer client)))
               (cl-incf (buffer-local-value 'exwm--ping buf))))
          (t (exwm--log "Unhandled WM_PROTOCOLS of type: %d" type)))))

(defun exwm--on-wm-change-state (id data)
  "Handle WM_CHANGE_STATE message with ID and DATA."
  (let ((buffer (exwm--id->buffer id)))
    (when (and (buffer-live-p buffer)
               (= (elt data 0) xcb:icccm:WM_STATE:IconicState))
      (with-current-buffer buffer
        (if exwm--floating-frame
            (call-interactively #'exwm-floating-hide)
          ;; We can't simply bury the buffer because it may be in an unselected
          ;; window, or even a different frame/workspace.
          (replace-buffer-in-windows)))
      ;; We bury the buffer even if it's floating to ensure it goes to the
      ;; back of the buffer switch list.
      (bury-buffer buffer))))

(defun exwm--on-ClientMessage (raw-data _synthetic)
  "Handle ClientMessage event.
RAW-DATA contains unmarshalled ClientMessage event data."
  (let* ((obj (xcb:unmarshal-new 'xcb:ClientMessage raw-data))
         (type (slot-value obj 'type))
         (id (slot-value obj 'window))
         (data (slot-value (slot-value obj 'data) 'data32))
         (fn (alist-get type exwm--client-message-functions)))
    (if (not fn)
        (exwm--log "Unhandled: %s(%d)"
                   (x-get-atom-name type exwm-workspace--current) type)
      (exwm--log "atom=%s(%s) id=#x%x data=%s"
                 (x-get-atom-name type exwm-workspace--current)
                 type (or id 0) data)
      (funcall fn id data))))

(defun exwm--on-SelectionClear (data _synthetic)
  "Handle SelectionClear events.
DATA contains unmarshalled SelectionClear event data."
  (exwm--log)
  (with-slots (owner selection)
      (xcb:unmarshal-new 'xcb:SelectionClear data)
    (when (and (eq owner exwm--wmsn-window)
               (eq selection xcb:Atom:WM_S0))
      (exwm-wm-mode -1))))

(defun exwm--on-delete-terminal (terminal)
  "Handle terminal being deleted without Emacs being killed.
This function is Hooked to `delete-terminal-functions'.

TERMINAL is the terminal being (or that has been) deleted.

This may happen when invoking `save-buffers-kill-terminal' within an emacsclient
session."
  (when (eq terminal exwm--terminal)
    (exwm-wm-mode -1)))

(defun exwm--init-icccm-ewmh ()
  "Initialize ICCCM/EWMH support."
  (exwm--log)
  ;; Handle PropertyNotify event
  (xcb:+event exwm--connection 'xcb:PropertyNotify #'exwm--on-PropertyNotify)
  ;; Handle relevant client messages
  (xcb:+event exwm--connection 'xcb:ClientMessage #'exwm--on-ClientMessage)
  ;; Handle SelectionClear
  (xcb:+event exwm--connection 'xcb:SelectionClear #'exwm--on-SelectionClear)
  ;; Set _NET_SUPPORTED
  (xcb:+request exwm--connection
      (make-instance 'xcb:ewmh:set-_NET_SUPPORTED
                     :window exwm--root
                     :data (vector
                            ;; Root windows properties.
                            xcb:Atom:_NET_SUPPORTED
                            xcb:Atom:_NET_CLIENT_LIST
                            xcb:Atom:_NET_CLIENT_LIST_STACKING
                            xcb:Atom:_NET_NUMBER_OF_DESKTOPS
                            xcb:Atom:_NET_DESKTOP_GEOMETRY
                            xcb:Atom:_NET_DESKTOP_VIEWPORT
                            xcb:Atom:_NET_CURRENT_DESKTOP
                            ;; xcb:Atom:_NET_DESKTOP_NAMES
                            xcb:Atom:_NET_ACTIVE_WINDOW
                            ;; xcb:Atom:_NET_WORKAREA
                            xcb:Atom:_NET_SUPPORTING_WM_CHECK
                            ;; xcb:Atom:_NET_VIRTUAL_ROOTS
                            ;; xcb:Atom:_NET_DESKTOP_LAYOUT
                            ;; xcb:Atom:_NET_SHOWING_DESKTOP

                            ;; Other root window messages.
                            xcb:Atom:_NET_CLOSE_WINDOW
                            ;; xcb:Atom:_NET_MOVERESIZE_WINDOW
                            xcb:Atom:_NET_WM_MOVERESIZE
                            ;; xcb:Atom:_NET_RESTACK_WINDOW
                            xcb:Atom:_NET_REQUEST_FRAME_EXTENTS

                            ;; Application window properties.
                            xcb:Atom:_NET_WM_NAME
                            ;; xcb:Atom:_NET_WM_VISIBLE_NAME
                            ;; xcb:Atom:_NET_WM_ICON_NAME
                            ;; xcb:Atom:_NET_WM_VISIBLE_ICON_NAME
                            xcb:Atom:_NET_WM_DESKTOP
                            ;;
                            xcb:Atom:_NET_WM_WINDOW_TYPE
                            ;; xcb:Atom:_NET_WM_WINDOW_TYPE_DESKTOP
                            xcb:Atom:_NET_WM_WINDOW_TYPE_DOCK
                            xcb:Atom:_NET_WM_WINDOW_TYPE_TOOLBAR
                            xcb:Atom:_NET_WM_WINDOW_TYPE_MENU
                            xcb:Atom:_NET_WM_WINDOW_TYPE_UTILITY
                            xcb:Atom:_NET_WM_WINDOW_TYPE_SPLASH
                            xcb:Atom:_NET_WM_WINDOW_TYPE_DIALOG
                            xcb:Atom:_NET_WM_WINDOW_TYPE_DROPDOWN_MENU
                            xcb:Atom:_NET_WM_WINDOW_TYPE_POPUP_MENU
                            xcb:Atom:_NET_WM_WINDOW_TYPE_TOOLTIP
                            xcb:Atom:_NET_WM_WINDOW_TYPE_NOTIFICATION
                            xcb:Atom:_NET_WM_WINDOW_TYPE_COMBO
                            xcb:Atom:_NET_WM_WINDOW_TYPE_DND
                            xcb:Atom:_NET_WM_WINDOW_TYPE_NORMAL
                            ;;
                            xcb:Atom:_NET_WM_STATE
                            ;; xcb:Atom:_NET_WM_STATE_MODAL
                            ;; xcb:Atom:_NET_WM_STATE_STICKY
                            ;; xcb:Atom:_NET_WM_STATE_MAXIMIZED_VERT
                            ;; xcb:Atom:_NET_WM_STATE_MAXIMIZED_HORZ
                            ;; xcb:Atom:_NET_WM_STATE_SHADED
                            ;; xcb:Atom:_NET_WM_STATE_SKIP_TASKBAR
                            ;; xcb:Atom:_NET_WM_STATE_SKIP_PAGER
                            xcb:Atom:_NET_WM_STATE_HIDDEN
                            xcb:Atom:_NET_WM_STATE_FULLSCREEN
                            ;; xcb:Atom:_NET_WM_STATE_ABOVE
                            ;; xcb:Atom:_NET_WM_STATE_BELOW
                            xcb:Atom:_NET_WM_STATE_DEMANDS_ATTENTION
                            ;; xcb:Atom:_NET_WM_STATE_FOCUSED
                            ;;
                            xcb:Atom:_NET_WM_ALLOWED_ACTIONS
                            xcb:Atom:_NET_WM_ACTION_MOVE
                            xcb:Atom:_NET_WM_ACTION_RESIZE
                            xcb:Atom:_NET_WM_ACTION_MINIMIZE
                            ;; xcb:Atom:_NET_WM_ACTION_SHADE
                            ;; xcb:Atom:_NET_WM_ACTION_STICK
                            ;; xcb:Atom:_NET_WM_ACTION_MAXIMIZE_HORZ
                            ;; xcb:Atom:_NET_WM_ACTION_MAXIMIZE_VERT
                            xcb:Atom:_NET_WM_ACTION_FULLSCREEN
                            xcb:Atom:_NET_WM_ACTION_CHANGE_DESKTOP
                            xcb:Atom:_NET_WM_ACTION_CLOSE
                            ;; xcb:Atom:_NET_WM_ACTION_ABOVE
                            ;; xcb:Atom:_NET_WM_ACTION_BELOW
                            ;;
                            xcb:Atom:_NET_WM_STRUT
                            xcb:Atom:_NET_WM_STRUT_PARTIAL
                            ;; xcb:Atom:_NET_WM_ICON_GEOMETRY
                            xcb:Atom:_NET_WM_ICON
                            xcb:Atom:_NET_WM_PID
                            ;; xcb:Atom:_NET_WM_HANDLED_ICONS
                            ;; xcb:Atom:_NET_WM_USER_TIME
                            ;; xcb:Atom:_NET_WM_USER_TIME_WINDOW
                            xcb:Atom:_NET_FRAME_EXTENTS
                            ;; xcb:Atom:_NET_WM_OPAQUE_REGION
                            ;; xcb:Atom:_NET_WM_BYPASS_COMPOSITOR

                            ;; Window manager protocols.
                            xcb:Atom:_NET_WM_PING
                            ;; xcb:Atom:_NET_WM_SYNC_REQUEST
                            ;; xcb:Atom:_NET_WM_FULLSCREEN_MONITORS

                            ;; Other properties.
                            xcb:Atom:_NET_WM_FULL_PLACEMENT)))
  ;; Create a child window for setting _NET_SUPPORTING_WM_CHECK
  (let ((new-id (xcb:generate-id exwm--connection)))
    (setq exwm--guide-window new-id)
    (xcb:+request exwm--connection
        (make-instance 'xcb:CreateWindow
                       :depth 0
                       :wid new-id
                       :parent exwm--root
                       :x -1
                       :y -1
                       :width 1
                       :height 1
                       :border-width 0
                       :class xcb:WindowClass:InputOnly
                       :visual 0
                       :value-mask xcb:CW:OverrideRedirect
                       :override-redirect 1))
    ;; Set _NET_WM_NAME.  Must be set to the name of the window manager, as
    ;; required by wm-spec.
    (xcb:+request exwm--connection
        (make-instance 'xcb:ewmh:set-_NET_WM_NAME
                       :window new-id :data "EXWM"))
    (dolist (i (list exwm--root new-id))
      ;; Set _NET_SUPPORTING_WM_CHECK
      (xcb:+request exwm--connection
          (make-instance 'xcb:ewmh:set-_NET_SUPPORTING_WM_CHECK
                         :window i :data new-id))))
  ;; Set _NET_DESKTOP_VIEWPORT (we don't support large desktop).
  (xcb:+request exwm--connection
      (make-instance 'xcb:ewmh:set-_NET_DESKTOP_VIEWPORT
                     :window exwm--root
                     :data [0 0]))
  (xcb:flush exwm--connection))

(defun exwm--wmsn-acquire (replace)
  "Acquire the WM_Sn selection.

REPLACE specifies what to do in case there already is a window
manager.  If t, replace it, if nil, abort and ask the user if `ask'."
  (exwm--log "%s" replace)
  (with-slots (owner)
      (xcb:+request-unchecked+reply exwm--connection
          (make-instance 'xcb:GetSelectionOwner
                         :selection xcb:Atom:WM_S0))
    (when (/= owner xcb:Window:None)
      (when (eq replace 'ask)
        (setq replace (yes-or-no-p "Replace existing window manager? ")))
      (unless replace
        (error "Other window manager detected")))
    (let ((new-owner (xcb:generate-id exwm--connection)))
      (xcb:+request exwm--connection
          (make-instance 'xcb:CreateWindow
                         :depth 0
                         :wid new-owner
                         :parent exwm--root
                         :x -1
                         :y -1
                         :width 1
                         :height 1
                         :border-width 0
                         :class xcb:WindowClass:CopyFromParent
                         :visual 0
                         :value-mask 0
                         :override-redirect 0))
      (xcb:+request exwm--connection
          (make-instance 'xcb:ewmh:set-_NET_WM_NAME
                         :window new-owner :data "EXWM: exwm--wmsn-window"))
      (xcb:+request-checked+request-check exwm--connection
          (make-instance 'xcb:SetSelectionOwner
                         :selection xcb:Atom:WM_S0
                         :owner new-owner
                         :time xcb:Time:CurrentTime))
      (with-slots (owner)
          (xcb:+request-unchecked+reply exwm--connection
              (make-instance 'xcb:GetSelectionOwner
                             :selection xcb:Atom:WM_S0))
        (unless (eq owner new-owner)
          (error "Could not acquire ownership of WM selection")))
      ;; Wait for the other window manager to terminate.
      (when (/= owner xcb:Window:None)
        (let (reply)
          (cl-dotimes (i exwm--wmsn-acquire-timeout)
            (setq reply (xcb:+request-unchecked+reply exwm--connection
                            (make-instance 'xcb:GetGeometry :drawable owner)))
            (unless reply
              (cl-return))
            (message "Waiting for other window manager to quit... %ds" i)
            (sleep-for 1))
          (when reply
            (error "Other window manager did not release selection in time"))))
      ;; announce
      (let* ((cmd (make-instance 'xcb:ClientMessageData
                                 :data32 (vector xcb:Time:CurrentTime
                                                 xcb:Atom:WM_S0
                                                 new-owner
                                                 0
                                                 0)))
             (cm (make-instance 'xcb:ClientMessage
                                               :window exwm--root
                                               :format 32
                                               :type xcb:Atom:MANAGER
                                               :data cmd))
             (se (make-instance 'xcb:SendEvent
                         :propagate 0
                         :destination exwm--root
                         :event-mask xcb:EventMask:NoEvent
                         :event (xcb:marshal cm exwm--connection))))
        (xcb:+request exwm--connection se))
      (setq exwm--wmsn-window new-owner))))

(defun exwm--init-xcursor ()
  "Initialize Xcursor for connection.
Set the default cursor to `left_ptr'"
  (xcb:cursor:init exwm--connection)
  (xcb:+request exwm--connection
      (make-instance 'xcb:ChangeWindowAttributes
                     :window exwm--root
                     :value-mask xcb:CW:Cursor
                     :cursor (xcb:cursor:load-cursor exwm--connection "left_ptr"))))

(cl-defun exwm--init (&optional frame)
  "Initialize EXWM.
FRAME, if given, indicates the X display EXWM should manage."
  (exwm--log "%s" frame)
  (cl-assert (not exwm--connection))
  (setq frame (or frame (exwm--find-x-frame)))
  ;; The frame might not be selected if it's created by emacsclient.
  (select-frame-set-input-focus frame)
  (condition-case err
      (progn
        (unless (eq 'x (framep frame))
          (error "Not running under X environment"))
        ;; Never initialize again
        (remove-hook 'window-setup-hook #'exwm--init)
        (remove-hook 'after-make-frame-functions #'exwm--init)
        (setq exwm--terminal (frame-terminal frame))
        (setq exwm--connection (xcb:connect))
        (set-process-query-on-exit-flag (slot-value exwm--connection 'process)
                                        nil) ;prevent query message on exit
        (setq exwm--root
              (slot-value (car (slot-value
                                (xcb:get-setup exwm--connection) 'roots))
                          'root))
        ;; Initialize ICCCM/EWMH support
        (xcb:icccm:init exwm--connection t)
        (xcb:ewmh:init exwm--connection t)
        (setq
         exwm--client-message-functions
         (list (cons xcb:Atom:_NET_NUMBER_OF_DESKTOPS #'exwm--on-net-number-of-desktops)
               (cons xcb:Atom:_NET_CURRENT_DESKTOP #'exwm--on-net-current-desktop)
               (cons xcb:Atom:_NET_ACTIVE_WINDOW #'exwm--on-net-active-window)
               (cons xcb:Atom:_NET_CLOSE_WINDOW #'exwm--on-net-close-window)
               (cons xcb:Atom:_NET_REQUEST_FRAME_EXTENTS
                     #'exwm--on-net-request-frame-extents)
               (cons xcb:Atom:_NET_WM_DESKTOP #'exwm--on-net-wm-desktop)
               (cons xcb:Atom:_NET_WM_STATE #'exwm--on-net-wm-state)
               (cons xcb:Atom:WM_PROTOCOLS #'exwm--on-wm-protocols)
               (cons xcb:Atom:WM_CHANGE_STATE #'exwm--on-wm-change-state)))
        ;; Try to register window manager selection.
        (exwm--wmsn-acquire exwm-replace)
        (when (xcb:+request-checked+request-check exwm--connection
                  (make-instance 'xcb:ChangeWindowAttributes
                                 :window exwm--root
                                 :value-mask xcb:CW:EventMask
                                 :event-mask
                                 xcb:EventMask:SubstructureRedirect))
          (error "Other window manager is running"))
        ;; Disable some features not working well with EXWM
        (setq use-dialog-box nil
              confirm-kill-emacs #'exwm--confirm-kill-emacs)
        (advice-add 'save-buffers-kill-terminal
                    :before-while #'exwm--confirm-kill-terminal)
        ;; Clean up if the terminal is deleted.
        (add-hook 'delete-terminal-functions 'exwm--on-delete-terminal)
        (exwm--lock)
        (exwm--init-icccm-ewmh)
        (exwm--init-xcursor)
        (exwm-layout--init)
        (exwm-floating--init)
        (exwm-manage--init)
        (exwm-workspace--init)
        (exwm-input--init)
        (exwm--unlock)
        (exwm-workspace--post-init)
        (exwm-input--post-init)
        (run-hooks 'exwm-init-hook)
        ;; Manage existing windows
        (exwm-manage--scan))
    ((quit error)
     (exwm-wm-mode -1)
     ;; Rethrow error
     (warn "[EXWM] EXWM fails to start (%s: %s)" (car err) (cdr err)))))

(defun exwm--exit ()
  "Exit EXWM."
  (exwm--log)
  (cl-assert exwm--connection)
  (run-hooks 'exwm-exit-hook)
  (setq confirm-kill-emacs nil)
  ;; Exit modules.
  (exwm-input--exit)
  (exwm-manage--exit)
  (exwm-workspace--exit)
  (exwm-floating--exit)
  (exwm-layout--exit)
  (xcb:flush exwm--connection)
  (xcb:disconnect exwm--connection)
  (setq exwm--connection nil
        exwm--terminal nil)
  (exwm--log "Exited"))

;;;###autoload
(define-minor-mode exwm-wm-mode
  "EXWM window manager mode."
  :global t
  :group 'exwm
  (if exwm-wm-mode
      (unless exwm--connection
        (exwm--enable)
        (when-let* ((frame (and
                            ;; If we're currently setting up the initial frame, don't start EXWM
                            ;; yet. Wait for `window-setup-hook' to run.
                            (not frame-initial-frame)
                            ;; Otherwise, if we already have a frame, start EXWM immediately.
                            (exwm--find-x-frame))))
          (exwm--init frame)))
    (when exwm--connection
      (exwm--exit))
    (exwm--disable)))

(defun exwm--disable ()
  "Unregister functions for EXWM to be initialized."
  (exwm--log)
  (setq x-no-window-manager nil)
  (setenv "INSIDE_EXWM" nil)
  (remove-hook 'window-setup-hook #'exwm--init)
  (remove-hook 'after-make-frame-functions #'exwm--init)
  (remove-hook 'kill-emacs-hook #'exwm--server-stop)
  (dolist (i exwm-blocking-subrs)
    (advice-remove i #'exwm--server-eval-at)))

(defun exwm--enable ()
  "Register functions for EXWM to be initialized."
  (exwm--log)
  (setq frame-resize-pixelwise t     ;mandatory; before init
        window-resize-pixelwise t
        x-no-window-manager t)
  (setenv "INSIDE_EXWM" "1")
  (if (eq initial-window-system 'x)
      ;; In case EXWM is to be started from a graphical Emacs instance.
      (add-hook 'window-setup-hook #'exwm--init t)
    ;; In case EXWM is to be started with emacsclient.
    (add-hook 'after-make-frame-functions #'exwm--init t))
  ;; Manage the subordinate Emacs server.
  (add-hook 'kill-emacs-hook #'exwm--server-stop)
  (dolist (i exwm-blocking-subrs)
    (advice-add i :around #'exwm--server-eval-at)))

;;;###autoload
(defun exwm-enable (&optional undo)
  "Obsolete function to enable/disable EXWM, use `exwm-wm-mode' instead.
Optional argument UNDO may be either of the following symbols:
- `undo' prevents reinitialization.
- `undo-all' attempts to revert all hooks and advice."
  (declare (obsolete exwm-wm-mode "0.33"))
  (message "EXWM: Use `exwm-wm-mode' instead of the obsolete `exwm-enable'.")
  (pcase undo
    (`undo (remove-hook 'window-setup-hook #'exwm--init)
           (remove-hook 'after-make-frame-functions #'exwm--init))
    (`undo-all (exwm-wm-mode -1))
    (_ (exwm-wm-mode 1))))

(defun exwm--server-stop ()
  "Stop the subordinate Emacs server."
  (exwm--log)
  (when exwm--server-process
    (when (process-live-p exwm--server-process)
      (cl-loop
       initially (signal-process exwm--server-process 'TERM)
       while     (process-live-p exwm--server-process)
       repeat    (* 10 exwm--server-timeout)
       do        (sit-for 0.1)))
    (delete-process exwm--server-process)
    (setq exwm--server-process nil)))

(defun exwm--server-eval-at (function &rest args)
  "Wrapper of `server-eval-at' used to advice subrs.
FUNCTION is the function to be evaluated, ARGS are the arguments."
  ;; Start the subordinate Emacs server if it's not alive
  (exwm--log "%s %s" function args)
  (unless (server-running-p exwm--server-name)
    (when exwm--server-process (delete-process exwm--server-process))
    (setq exwm--server-process
          (start-process exwm--server-name
                         nil
                         (car command-line-args) ;The executable file
                         "-d" (frame-parameter nil 'display)
                         "-Q"
                         (concat "--fg-daemon=" exwm--server-name)
                         "--eval"
                         ;; Create an invisible frame
                         "(make-frame '((window-system . x) (visibility)))"))
    (while (not (server-running-p exwm--server-name))
      (sit-for 0.001)))
  (server-eval-at
   exwm--server-name
   `(progn (select-frame (car (frame-list)))
           (let ((result ,(nconc (list (make-symbol (subr-name function)))
                                 args)))
             (pcase (type-of result)
               ;; Return the name of a buffer
               (`buffer (buffer-name result))
               ;; We blindly convert all font objects to their XLFD names. This
               ;; might cause problems of course, but it still has a chance to
               ;; work (whereas directly passing font objects would merely
               ;; raise errors).
               ((or `font-entity `font-object `font-spec)
                (font-xlfd-name result))
               ;; Passing following types makes little sense
               ((or `compiled-function `finalizer `frame `hash-table `marker
                    `overlay `process `window `window-configuration))
               ;; Passing the name of a subr
               (`subr (make-symbol (subr-name result)))
               ;; For other types, return the value as-is.
               (t result))))))

(defun exwm--confirm-kill-terminal (&optional _)
  "Confirm before killing terminal."
  ;; This is invoked instead of `save-buffers-kill-emacs' (C-x C-c) on client
  ;; frames.
  (if (exwm--terminal-p)
      (exwm--confirm-kill-emacs "Kill terminal")
    t))

(defun exwm--confirm-kill-emacs (prompt &optional force)
  "Confirm before exiting Emacs.
PROMPT a reason to present to the user.
If FORCE is nil, ask the user for confirmation.
If FORCE is the symbol `no-check', ask if there are unsaved buffers.
If FORCE is any other non-nil value, force killing of Emacs."
  (exwm--log)
  (when (cond
         ((and force (not (eq force 'no-check)))
          ;; Force killing Emacs.
          t)
         ((or (eq force 'no-check) (not exwm--id-buffer-alist))
          ;; Check if there's any unsaved file.
          (pcase (catch 'break
                   (let ((kill-emacs-query-functions
                          (append kill-emacs-query-functions
                                  (list (lambda ()
                                          (throw 'break 'break))))))
                     (save-buffers-kill-emacs)))
            (`break (y-or-n-p prompt))
            (x x)))
         (t
          (yes-or-no-p (format "[EXWM] %d X window(s) will be destroyed.  %s?"
                               (length exwm--id-buffer-alist) prompt))))
    ;; Run `kill-emacs-hook' (`server-force-stop' excluded) before Emacs
    ;; frames are unmapped so that errors (if any) can be visible.
    (if (memq #'server-force-stop kill-emacs-hook)
        (progn
          (setq kill-emacs-hook (delq #'server-force-stop kill-emacs-hook))
          (run-hooks 'kill-emacs-hook)
          (setq kill-emacs-hook (list #'server-force-stop)))
      (run-hooks 'kill-emacs-hook)
      (setq kill-emacs-hook nil))
    ;; Exit each module, destroying all resources created by this connection.
    (exwm-wm-mode -1)
    ;; Set the return value.
    t))

(define-obsolete-function-alias 'exwm-init #'exwm--init "0.33")
(define-obsolete-function-alias 'exwm-exit #'exwm--exit "0.33")

(provide 'exwm)
;;; exwm.el ends here

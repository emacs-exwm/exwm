;;; exwm-core.el --- Core definitions  -*- lexical-binding: t -*-

;; Copyright (C) 2015-2025 Free Software Foundation, Inc.

;; Author: Chris Feng <chris.w.feng@gmail.com>

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

;; This module includes core definitions of variables, macros, functions, etc
;; shared by various other modules.

;;; Code:

(require 'compat)
(require 'kmacro)

(require 'xcb)
(require 'xcb-icccm)
(require 'xcb-ewmh)
(require 'xcb-debug)

(defgroup exwm-debug nil
  "Debugging."
  :group 'exwm)

(defcustom exwm-debug-log-time-function #'exwm-debug-log-uptime
  "Function used for generating timestamps in debug log.

Here are some predefined candidates:
`exwm-debug-log-uptime': Display the uptime of this Emacs instance.
`exwm-debug-log-time': Display time of day.
nil: Disable timestamp."
  :type `(choice (const :tag "Emacs uptime" ,#'exwm-debug-log-uptime)
                 (const :tag "Time of day" ,#'exwm-debug-log-time)
                 (const :tag "Off" nil)
                 (function :tag "Other"))
  :set (lambda (symbol value)
         (set-default symbol value)
         ;; Also change the format for XELB to make logs consistent
         ;; (as they share the same buffer).
         (setq xcb-debug:log-time-function value)))

(defalias 'exwm-debug-log-uptime 'xcb-debug:log-uptime
  "Add uptime to `exwm-debug' logs.")

(defalias 'exwm-debug-log-time 'xcb-debug:log-time
  "Add time of day to `exwm-debug' logs.")

(defvar exwm--connection nil "X connection.")

(defvar exwm--terminal nil
  "Terminal corresponding to `exwm--connection'.")

(defvar exwm--wmsn-window nil
  "An X window owning the WM_S0 selection.")

(defvar exwm--wmsn-acquire-timeout 3
  "Number of seconds to wait for other window managers to release the selection.")

(defvar exwm--guide-window nil
  "An X window separating workspaces and X windows.")

(defvar exwm--id-buffer-alist nil "Alist of (<X window ID> . <Emacs buffer>).")

(defvar exwm--root nil "Root window.")

(defvar exwm-input--global-prefix-keys)
(defvar exwm-input--simulation-keys)
(defvar exwm-input-line-mode-passthrough)
(defvar exwm-input-prefix-keys)
(defvar exwm-workspace--list)
(declare-function exwm-input--fake-key "exwm-input.el" (event))
(declare-function exwm-input--on-KeyPress-line-mode "exwm-input.el"
                  (key-press raw-data))
(declare-function exwm-floating-hide "exwm-floating.el")
(declare-function exwm-floating-toggle-floating "exwm-floating.el")
(declare-function exwm-input-release-keyboard "exwm-input.el")
(declare-function exwm-input-send-next-key "exwm-input.el" (times))
(declare-function exwm-layout-set-fullscreen "exwm-layout.el" (&optional id))
(declare-function exwm-layout-toggle-mode-line "exwm-layout.el")
(declare-function exwm-manage--kill-buffer-query-function "exwm-manage.el")
(declare-function exwm-workspace-move-window "exwm-workspace.el"
                  (frame-or-index &optional id))
(declare-function exwm-workspace-switch "exwm-workspace.el"
                  (frame-or-index &optional force))

(defvaralias 'exwm-debug 'exwm-debug-mode)
(define-minor-mode exwm-debug-mode
  "Debug-logging enabled if non-nil."
  :global t
  :group 'exwm-debug)

(defmacro exwm--debug (&rest forms)
  "Evaluate FORMS if `exwm-debug-mode' is active."
  (when exwm-debug `(progn ,@forms)))

(defmacro exwm--log (&optional format-string &rest objects)
  "Emit a message prepending the name of the function being executed.

FORMAT-STRING is a string specifying the message to output, as in
`format'.  The OBJECTS arguments specify the substitutions."
  (unless format-string (setq format-string ""))
  `(when exwm-debug
     (xcb-debug:message ,(concat "%s%s:\t" format-string "\n")
                        (if exwm-debug-log-time-function
                            (funcall exwm-debug-log-time-function)
                          "")
                        (xcb-debug:compile-time-function-name)
                        ,@objects)
     nil))

(defsubst exwm--id->buffer (id)
  "X window ID => Emacs buffer."
  (declare (indent defun))
  (cdr (assoc id exwm--id-buffer-alist)))

(defsubst exwm--buffer->id (buffer)
  "Emacs buffer BUFFER => X window ID."
  (declare (indent defun))
  (car (rassoc buffer exwm--id-buffer-alist)))

(defun exwm--lock (&rest _args)
  "Lock (disable all events)."
  (exwm--log)
  (xcb:+request exwm--connection
      (make-instance 'xcb:ChangeWindowAttributes
                     :window exwm--root
                     :value-mask xcb:CW:EventMask
                     :event-mask xcb:EventMask:NoEvent))
  (xcb:flush exwm--connection))

(defun exwm--unlock (&rest _args)
  "Unlock (enable all events)."
  (exwm--log)
  (xcb:+request exwm--connection
      (make-instance 'xcb:ChangeWindowAttributes
                     :window exwm--root
                     :value-mask xcb:CW:EventMask
                     :event-mask (eval-when-compile
                                   (logior xcb:EventMask:SubstructureRedirect
                                           xcb:EventMask:StructureNotify))))
  (xcb:flush exwm--connection))

(defun exwm--set-geometry (xwin x y width height)
  "Set the geometry of X window XWIN to WIDTHxHEIGHT+X+Y.

Nil can be passed as placeholder."
  (exwm--log "Setting #x%x to %sx%s+%s+%s" xwin width height x y)
  (xcb:+request exwm--connection
      (make-instance 'xcb:ConfigureWindow
                     :window xwin
                     :value-mask (logior (if x xcb:ConfigWindow:X 0)
                                         (if y xcb:ConfigWindow:Y 0)
                                         (if width xcb:ConfigWindow:Width 0)
                                         (if height xcb:ConfigWindow:Height 0))
                     :x x :y y :width width :height height)))

(defun exwm--intern-atom (atom &optional conn)
  "Intern X11 ATOM.
If CONN is non-nil, use it instead of the value of the variable
`exwm--connection'."
  (slot-value (xcb:+request-unchecked+reply (or conn exwm--connection)
                  (make-instance 'xcb:InternAtom
                                 :only-if-exists 0
                                 :name-len (length atom)
                                 :name atom))
              'atom))

(defmacro exwm--defer (secs function &rest args)
  "Defer the execution of FUNCTION.

The action is to call FUNCTION with arguments ARGS.  If Emacs is not idle,
defer the action until Emacs is idle.  Otherwise, defer the action until at
least SECS seconds later."
  `(run-with-idle-timer (+ (float-time (or (current-idle-time)
                                           (seconds-to-time (- ,secs))))
                           ,secs)
                        nil
                        ,function
                        ,@args))

(defsubst exwm--terminal-p (&optional frame)
  "Return t when FRAME's terminal is EXWM's terminal.
If FRAME is null, use selected frame."
  (declare (indent defun))
  (eq exwm--terminal (frame-terminal frame)))

(defun exwm--get-client-event-mask ()
  "Return event mask set on all managed windows."
  (logior xcb:EventMask:StructureNotify
          xcb:EventMask:PropertyChange
          (if mouse-autoselect-window
              xcb:EventMask:EnterWindow 0)))

(defun exwm--color->pixel (color)
  "Convert COLOR to PIXEL (index in TrueColor colormap)."
  (when (and color
             (eq (x-display-visual-class) 'true-color))
    (let ((rgb (color-values color)))
      (logior (ash (ash (pop rgb) -8) 16)
              (ash (ash (pop rgb) -8) 8)
              (ash (pop rgb) -8)))))

(defun exwm--get-visual-depth-colormap (conn id)
  "Get visual, depth and colormap from X window ID.
Return a three element list with the respective results.

If CONN is non-nil, use it instead of the value of the variable
`exwm--connection'."
  (let (ret-visual ret-depth ret-colormap)
    (with-slots (visual colormap)
        (xcb:+request-unchecked+reply conn
            (make-instance 'xcb:GetWindowAttributes :window id))
      (setq ret-visual visual)
      (setq ret-colormap colormap))
    (with-slots (depth)
        (xcb:+request-unchecked+reply conn
            (make-instance 'xcb:GetGeometry :drawable id))
      (setq ret-depth depth))
    (list ret-visual ret-depth ret-colormap)))

(defun exwm--mode-name ()
  "Mode name string used in `exwm-mode' buffers."
  (let ((name "EXWM"))
    (if (cl-some (lambda (i) (frame-parameter i 'exwm-urgency))
                 exwm-workspace--list)
        (propertize name 'face 'font-lock-warning-face)
      name)))

;; Internal variables
(defvar-local exwm--id nil)               ;window ID
(defvar-local exwm--configurations nil)   ;initial configurations.
(defvar-local exwm--frame nil)            ;workspace frame
(defvar-local exwm--floating-frame nil)   ;floating frame
(defvar-local exwm--mode-line-format nil) ;save mode-line-format
(defvar-local exwm--floating-frame-position nil) ;set when hidden.
(defvar-local exwm--fixed-size nil)              ;fixed size
(defvar-local exwm--selected-input-mode 'line-mode
  "Input mode as selected by the user.
One of `line-mode' or `char-mode'.")
(defvar-local exwm--input-mode 'line-mode
  "Actual input mode, i.e. whether mouse and keyboard are grabbed.")
;; Properties
(defvar-local exwm--desktop nil "_NET_WM_DESKTOP.")
(defvar-local exwm-window-type nil "_NET_WM_WINDOW_TYPE.")
(defvar-local exwm--geometry nil)
(defvar-local exwm-class-name nil "Class name in WM_CLASS.")
(defvar-local exwm-instance-name nil "Instance name in WM_CLASS.")
(defvar-local exwm-title nil "Window title (either _NET_WM_NAME or WM_NAME).")
(defvar-local exwm--title-is-utf8 nil)
(defvar-local exwm-transient-for nil "WM_TRANSIENT_FOR.")
(defvar-local exwm--protocols nil)
(defvar-local exwm-state xcb:icccm:WM_STATE:NormalState "WM_STATE.")
(defvar-local exwm--ewmh-state nil "_NET_WM_STATE.")
;; _NET_WM_NORMAL_HINTS
(defvar-local exwm--normal-hints-x nil)
(defvar-local exwm--normal-hints-y nil)
(defvar-local exwm--normal-hints-width nil)
(defvar-local exwm--normal-hints-height nil)
(defvar-local exwm--normal-hints-min-width nil)
(defvar-local exwm--normal-hints-min-height nil)
(defvar-local exwm--normal-hints-max-width nil)
(defvar-local exwm--normal-hints-max-height nil)
;; (defvar-local exwm--normal-hints-win-gravity nil)
;; WM_HINTS
(defvar-local exwm--hints-input nil)
(defvar-local exwm--hints-urgency nil)
;; _MOTIF_WM_HINTS
(defvar-local exwm--mwm-hints-decorations t)

(defvar-keymap exwm-mode-map
  :doc "Keymap for `exwm-mode'."
  "C-c C-d C-l" #'xcb-debug:clear
  "C-c C-d C-m" #'xcb-debug:mark
  "C-c C-d C-t" #'exwm-debug-mode
  "C-c C-f" #'exwm-layout-set-fullscreen
  "C-c C-h" #'exwm-floating-hide
  "C-c C-k" #'exwm-input-release-keyboard
  "C-c C-m" #'exwm-workspace-move-window
  "C-c C-q" #'exwm-input-send-next-key
  "C-c C-t C-f" #'exwm-floating-toggle-floating
  "C-c C-t C-m" #'exwm-layout-toggle-mode-line)

(defun exwm--kmacro-self-insert-command ()
  "The EXWM kmacro equivalent of `self-insert-command'."
  (interactive)
  (cond
   ((or exwm-input-line-mode-passthrough
        (active-minibuffer-window)
        (memq last-input-event exwm-input--global-prefix-keys)
        (memq last-input-event exwm-input-prefix-keys)
        (lookup-key exwm-mode-map (vector last-input-event))
        (gethash last-input-event exwm-input--simulation-keys))
    (set-transient-map (make-composed-keymap (list exwm-mode-map global-map)))
    (push last-input-event unread-command-events))
   (t
    (exwm-input--fake-key last-input-event))))
(put 'exwm--kmacro-self-insert-command 'completion-predicate #'ignore)

(defvar-keymap exwm--kmacro-map
  :doc "Keymap used when executing keyboard macros."
  "<t>" #'exwm--kmacro-self-insert-command)

;; This menu mainly acts as an reminder for users.  Thus it should be as
;; detailed as possible, even some entries do not make much sense here.
;; Also, inactive entries should be disabled rather than hidden.
(easy-menu-define exwm-mode-menu exwm-mode-map
  "Menu for `exwm-mode'."
  `("EXWM"
    ("General"
     ["Toggle floating" exwm-floating-toggle-floating]
     ["Toggle fullscreen mode" exwm-layout-toggle-fullscreen]
     ["Hide window" exwm-floating-hide exwm--floating-frame]
     ["Close window" (kill-buffer (current-buffer))])
    ("Resizing"
     ["Toggle mode-line" exwm-layout-toggle-mode-line]
     ["Enlarge window vertically" exwm-layout-enlarge-window]
     ["Enlarge window horizontally" exwm-layout-enlarge-window-horizontally]
     ["Shrink window vertically" exwm-layout-shrink-window]
     ["Shrink window horizontally" exwm-layout-shrink-window-horizontally])
    ("Keyboard"
     ["Toggle keyboard mode" exwm-input-toggle-keyboard]
     ["Send key" exwm-input-send-next-key (eq exwm--input-mode 'line-mode)]
    ;; This is merely a reference.
     ("Send simulation key" :filter
     ,(lambda (&rest _args)
        (let (result)
          (maphash
           (lambda (key value)
             (when (sequencep key)
               (setq result (append result
                                    `([,(format "Send '%s'"
                                                (key-description value))
                                       ,(lambda ()
                                          (interactive)
                                          (mapc #'exwm-input--fake-key value))
                                       :keys ,(key-description key)])))))
           exwm-input--simulation-keys)
          result)))

     ["Define global binding" exwm-input-set-key])

    ("Workspace"
     ["Add workspace" exwm-workspace-add]
     ["Delete current workspace" exwm-workspace-delete]
     ["Move workspace to" exwm-workspace-move]
     ["Swap workspaces" exwm-workspace-swap]
     ["Move X window to" exwm-workspace-move-window]
     ["Move X window from" exwm-workspace-switch-to-buffer]
     ["Toggle minibuffer" exwm-workspace-toggle-minibuffer]
     ["Switch workspace" exwm-workspace-switch]
     ;; Place this entry at bottom to avoid selecting others by accident.
     ("Switch to" :active (cdr exwm-workspace--list) :filter
      ,(lambda (&rest _args)
         (mapcar (lambda (i)
                   `[,(format "Workspace %d" i)
                     ,(lambda ()
                        (interactive)
                        (exwm-workspace-switch i))
                     (/= ,i exwm-workspace-current-index)])
                 (number-sequence 0 (1- (length exwm-workspace--list)))))))))

(define-derived-mode exwm-mode nil "EXWM"
  "Major mode for managing X windows.

\\{exwm-mode-map}"
  :interactive nil :abbrev-table nil :syntax-table nil
  ;; Change major-mode is not allowed
  (add-hook 'change-major-mode-hook #'kill-buffer nil t)
  ;; Kill buffer -> close window
  (add-hook 'kill-buffer-query-functions
            #'exwm-manage--kill-buffer-query-function nil t)
  ;; Redirect events when executing keyboard macros.
  (push `(executing-kbd-macro . ,exwm--kmacro-map)
        minor-mode-overriding-map-alist)
  (setq-local mode-name '(:eval (exwm--mode-name))
              buffer-read-only t
              cursor-type nil
              left-margin-width nil
              right-margin-width nil
              left-fringe-width 0
              right-fringe-width 0
              vertical-scroll-bar nil
              eldoc-documentation-functions nil
              mode-line-position nil
              mode-line-modified nil
              mode-line-mule-info nil
              mode-line-remote nil))

(defmacro exwm--global-minor-mode-body (name &optional init exit)
  "Global minor mode body for mode with NAME.
The INIT and EXIT functions are added to `exwm-init-hook' and
`exwm-exit-hook' respectively.  If an X connection exists, the mode is
immediately enabled or disabled."
  (declare (indent 1) (debug t))
  (let* ((mode (intern (format "exwm-%s-mode" name)))
         (init (or init (intern (format "exwm-%s--init" name))))
         (exit (or exit (intern (format "exwm-%s--exit" name)))))
    `(progn
       (exwm--log)
       (cond
        (,mode
         (add-hook 'exwm-init-hook #',init)
         (add-hook 'exwm-exit-hook #',exit)
         (when exwm--connection (,init)))
        (t
         (remove-hook 'exwm-init-hook #',init)
         (remove-hook 'exwm-exit-hook #',exit)
         (when exwm--connection (,exit)))))))

(provide 'exwm-core)
;;; exwm-core.el ends here

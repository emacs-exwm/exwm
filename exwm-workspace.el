;;; exwm-workspace.el --- Workspace Module for EXWM  -*- lexical-binding: t -*-

;; Copyright (C) 1015-2025 Free Software Foundation, Inc.

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

;; This module adds workspace support for EXWM.

;;; Code:

(require 'server)
(require 'exwm-core)
(eval-when-compile (require 'subr-x)) ;; Needed on 28 for when-let*

(defgroup exwm-workspace nil
  "Workspace."
  :group 'exwm)

(defcustom exwm-workspace-switch-hook nil
  "Normal hook run after switching workspace."
  :type 'hook)

(defcustom exwm-workspace-list-change-hook nil
  "Normal hook run when the workspace list is changed.
This happens when a workspace is added, deleted, moved, etc."
  :type 'hook)

(defcustom exwm-workspace-show-all-buffers nil
  "Non-nil to show buffers on other workspaces."
  :type 'boolean)

(defcustom exwm-workspace-warp-cursor nil
  "Non-nil to warp cursor automatically after workspace switch."
  :type 'boolean)

(defcustom exwm-workspace-number 1
  "Initial number of workspaces."
  :type 'integer)

(defvar exwm-workspace--switch-history nil
  "History for `read-from-minibuffer' to interactively switch workspace.")

(defvar exwm-workspace--switch-history-outdated nil
  "Non-nil to indicate `exwm-workspace--switch-history' is outdated.")

(defvar exwm-workspace--switch-by-index-map
  (let ((map (make-sparse-keymap)))
    (dotimes (i 10)
      (keymap-set map (int-to-string i)
                  #'exwm-workspace--switch-map-nth-prefix))
    map)
  "Keymap used to switch workspaces by index.")

(defvar exwm-workspace--switch-by-name-map (make-sparse-keymap)
  "Keymap used to switch workspaces by index.
Applicable when the name returned by `exwm-workspace-index-map' is also
a valid key.")

(defcustom exwm-workspace-index-map #'number-to-string
  "Function for mapping a workspace index to a string for display.

By default `number-to-string' is applied which yields 0 1 2 ... ."
  :type 'function
  :initialize 'custom-initialize-changed
  :set (lambda (symbol value)
         (set-default-toplevel-value symbol value)
         (setq exwm-workspace--switch-history-outdated t)
         (exwm-workspace--update-switch-by-name-map)))

(defcustom exwm-workspace-minibuffer-position nil
  "Position of the minibuffer frame.

A restart is required for this change to take effect."
  :type '(choice (const :tag "Bottom (fixed)" nil)
                 (const :tag "Bottom (auto-hide)" bottom)
                 (const :tag "Top (auto-hide)" top)))

(defcustom exwm-workspace-display-echo-area-timeout 1
  "Timeout for displaying echo area."
  :type 'integer)

(defcustom exwm-workspace-switch-create-limit 10
  "Number of workspaces `exwm-workspace-switch-create' is allowed to create."
  :type 'integer)

(defvar exwm-workspace-current-index 0 "Index of current active workspace.")

(defvar exwm-workspace--attached-minibuffer-height 0
  "Height (in pixel) of the attached minibuffer.

If the minibuffer is detached, this value is 0.")

(defvar exwm-workspace--create-silently nil
  "When non-nil workspaces are created in the background (not switched to).

Please manually run the hook `exwm-workspace-list-change-hook' afterwards.")

(defvar exwm-workspace--current nil "Current active workspace.")

(defvar exwm-workspace--display-echo-area-timer nil
  "Timer for auto-hiding echo area.")

(defvar exwm-workspace--id-struts-alist nil "Alist of X window and struts.")

(defvar exwm-workspace--fullscreen-frame-count 0
  "Count the fullscreen workspace frames.")

(defvar exwm-workspace--list nil "List of all workspaces (Emacs frames).")

(defvar exwm-workspace--minibuffer nil
  "The minibuffer frame shared among all frames.")

(defvar exwm-workspace--original-handle-focus-in
  (symbol-function #'handle-focus-in))
(defvar exwm-workspace--original-handle-focus-out
  (symbol-function #'handle-focus-out))

(defvar exwm-workspace--prompt-add-allowed nil
  "Non-nil to allow adding workspace from the prompt.")

(defvar exwm-workspace--prompt-delete-allowed nil
  "Non-nil to allow deleting workspace from the prompt.")

(defvar exwm-workspace--struts nil "Areas occupied by struts.")

(defvar exwm-workspace--timer nil "Timer used to track echo area changes.")

(defvar exwm-workspace--update-workareas-hook nil
  "Normal hook run when workareas get updated.")

(defvar exwm-workspace--workareas nil "Workareas (struts excluded).")

(defvar exwm-workspace--frame-y-offset 0
  "Offset between Emacs inner & outer frame in Y.")
(defvar exwm-workspace--window-y-offset 0
  "Offset between Emacs first window & outer frame in Y.")

(defvar exwm-input--event-hook)
(defvar exwm-layout-show-all-buffers)
(defvar exwm-manage--desktop)
(declare-function exwm-input--on-buffer-list-update "exwm-input.el" ())
(declare-function exwm-layout--fullscreen-p "exwm-layout.el" ())
(declare-function exwm-layout--hide "exwm-layout.el" (id))
(declare-function exwm-layout--other-buffer-predicate "exwm-layout.el"
                  (buffer))
(declare-function exwm-layout--refresh "exwm-layout.el")
(declare-function exwm-layout--show "exwm-layout.el" (id &optional window))

(defsubst exwm-workspace--position (frame)
  "Retrieve index of given FRAME in workspace list.
NIL if FRAME is not a workspace."
  (declare (indent defun))
  (cl-position frame exwm-workspace--list))

(defsubst exwm-workspace--count ()
  "Retrieve total number of workspaces."
  (length exwm-workspace--list))

(defsubst exwm-workspace--workspace-p (frame)
  "Return t if FRAME is a workspace."
  (declare (indent defun))
  (memq frame exwm-workspace--list))

(defsubst exwm-workspace--workarea (frame)
  "Return workarea corresponding to FRAME.
FRAME may be either a workspace frame or a workspace position."
  (declare (indent defun))
  (elt exwm-workspace--workareas
       (if (integerp frame)
           frame
         (exwm-workspace--position frame))))

(defvar-keymap exwm-workspace-switch-map
  :doc "Keymap used by `exwm-workspace-switch'."
  "+" #'exwm-workspace--prompt-add
  "-" #'exwm-workspace--prompt-delete
  "C-a" (lambda () (interactive) (goto-history-element 1))
  "C-e" (lambda () (interactive) (goto-history-element (exwm-workspace--count)))
  "C-g" #'abort-recursive-edit
  "C-]" #'abort-recursive-edit
  "C-j" #'exit-minibuffer
  "<return>" #'exit-minibuffer
  "<space>" #'exit-minibuffer
  "C-f" #'previous-history-element
  "C-b" #'next-history-element
  ;; Alternative keys
  "<right>" #'previous-history-element
  "<left>" #'next-history-element)

(defvar exwm-workspace--switch-composed-map
  (make-composed-keymap (list
                         exwm-workspace-switch-map
                         exwm-workspace--switch-by-name-map
                         exwm-workspace--switch-by-index-map)
                        (define-keymap "<t>" #'undefined))
  "Internal Keymap composing all the keymaps used by `exwm-workspace-switch'.")

(defun exwm-workspace--workspace-from-frame-or-index (frame-or-index)
  "Retrieve the workspace frame from FRAME-OR-INDEX."
  (cond
   ((framep frame-or-index)
    (unless (exwm-workspace--position frame-or-index)
      (user-error "[EXWM] Frame is not a workspace %S" frame-or-index))
    frame-or-index)
   ((integerp frame-or-index)
    (unless (and (<= 0 frame-or-index)
                 (< frame-or-index (exwm-workspace--count)))
      (user-error "[EXWM] Workspace index out of range: %d" frame-or-index))
    (elt exwm-workspace--list frame-or-index))
   (t (user-error "[EXWM] Invalid workspace: %s" frame-or-index))))

(defun exwm-workspace--prompt-for-workspace (&optional prompt)
  "Prompt for a workspace, returning the workspace frame.
Show PROMPT to the user if non-nil."
  (exwm-workspace--update-switch-history)
  (let* ((current-idx (exwm-workspace--position exwm-workspace--current))
         (history-add-new-input nil)  ;prevent modifying history
         (history-idx (read-from-minibuffer
                       (or prompt "Workspace: ")
                       (elt exwm-workspace--switch-history current-idx)
                       exwm-workspace--switch-composed-map nil
                       `(exwm-workspace--switch-history . ,(1+ current-idx))))
         (workspace-idx (cl-position history-idx exwm-workspace--switch-history
                                     :test #'equal)))
    (elt exwm-workspace--list workspace-idx)))

(defun exwm-workspace--prompt-add ()
  "Add workspace from the prompt."
  (interactive)
  (when exwm-workspace--prompt-add-allowed
    (let ((exwm-workspace--create-silently t))
      (make-frame)
      (run-hooks 'exwm-workspace-list-change-hook))
    (exwm-workspace--update-switch-history)
    (goto-history-element minibuffer-history-position)))
(put #'exwm-workspace--prompt-add 'completion-predicate #'ignore) ;; Move to declare in Emacs 28

(defun exwm-workspace--prompt-delete ()
  "Delete workspace from the prompt."
  (interactive)
  (when (and exwm-workspace--prompt-delete-allowed
             (< 1 (exwm-workspace--count)))
    (let ((frame (elt exwm-workspace--list (1- minibuffer-history-position))))
      (if (eq frame exwm-workspace--current)
          ;; Abort the recursive minibuffer if deleting the current workspace.
          (progn
            (exwm--defer 0 #'exwm-workspace-delete frame)
            (abort-recursive-edit))
        (exwm-workspace-delete frame)
        (exwm-workspace--update-switch-history)
        (goto-history-element (min minibuffer-history-position
                                   (exwm-workspace--count)))))))
(put #'exwm-workspace--prompt-delete 'completion-predicate #'ignore) ;; Move to declare in Emacs 28

(defun exwm-workspace--update-switch-history ()
  "Update the history for switching workspace to reflect the latest status."
  (when exwm-workspace--switch-history-outdated
    (setq exwm-workspace--switch-history-outdated nil)
    (let* ((num (exwm-workspace--count))
           (sequence (number-sequence 0 (1- num)))
           (not-empty (make-vector num nil)))
      (dolist (i exwm--id-buffer-alist)
        (with-current-buffer (cdr i)
          (when exwm--frame
            (setf (aref not-empty
                        (exwm-workspace--position exwm--frame))
                  t))))
      (setq exwm-workspace--switch-history
            (mapcar
             (lambda (i)
               (mapconcat
                (lambda (j)
                  (format (if (= i j) "[%s]" " %s ")
                          (propertize
                           (apply exwm-workspace-index-map (list j))
                           'face
                           (cond ((frame-parameter (elt exwm-workspace--list j)
                                                   'exwm-urgency)
                                  '(:foreground "orange"))
                                 ((aref not-empty j) '(:foreground "green"))
                                 (t nil)))))
                sequence ""))
             sequence)))))

(defun exwm-workspace--update-switch-by-name-map ()
  "Updates `exwm-workspace--switch-by-name-map'."
  (setcdr exwm-workspace--switch-by-name-map nil)
  (unless (eq exwm-workspace-index-map #'number-to-string)
    (dotimes (i 10)
      (let ((key (funcall exwm-workspace-index-map i)))
        (when (and (stringp key)
                   (length= key 1)
                   (<= 0 (elt key 0) 127))
          (keymap-set exwm-workspace--switch-by-name-map key
                      (lambda ()
                        (interactive)
                        (exwm-workspace--switch-map-select-nth i))))))))

(defun exwm-workspace--get-geometry (frame)
  "Return the geometry of frame FRAME."
  (or (frame-parameter frame 'exwm-geometry)
      (make-instance 'xcb:RECTANGLE
                     :x 0
                     :y 0
                     :width (x-display-pixel-width)
                     :height (x-display-pixel-height))))

(defun exwm-workspace--current-height ()
  "Return the height of current workspace."
  (let ((geometry (frame-parameter exwm-workspace--current 'exwm-geometry)))
    (if geometry
        (slot-value geometry 'height)
      (x-display-pixel-height))))

(defun exwm-workspace--minibuffer-own-frame-p ()
  "Reports whether the minibuffer is displayed in its own frame."
  (memq exwm-workspace-minibuffer-position '(top bottom)))

(defun exwm-workspace--update-struts ()
  "Update `exwm-workspace--struts'."
  (setq exwm-workspace--struts nil)
  (let (struts struts*)
    (dolist (pair exwm-workspace--id-struts-alist)
      (setq struts (cdr pair))
      (when struts
        (dotimes (i 4)
          (when (/= 0 (aref struts i))
            (setq struts*
                  (vector (aref [left right top bottom] i)
                          (aref struts i)
                          (when (= 12 (length struts))
                            (substring struts (+ 4 (* i 2)) (+ 6 (* i 2))))))
            (if (= 0 (mod i 2))
                ;; Make left/top processed first.
                (push struts* exwm-workspace--struts)
              (setq exwm-workspace--struts
                    (append exwm-workspace--struts (list struts*))))))))
    (exwm--log "%s" exwm-workspace--struts)))

(defun exwm-workspace--update-workareas ()
  "Update `exwm-workspace--workareas'."
  (let* ((root-width (x-display-pixel-width))
         (root-height (x-display-pixel-height))
         ;; Get workareas prior to struts.
         (workareas (mapcar
                     (lambda (frame)
                       (if-let* ((rect (frame-parameter frame 'exwm-geometry)))
                           ;; Use the 'exwm-geometry' frame parameter if it
                           ;; exists.  Make sure to clone it, will be modified
                           ;; below!
                           (clone rect)
                         ;; Fall back to use the screen size.
                         (make-instance 'xcb:RECTANGLE
                                        :x 0
                                        :y 0
                                        :width root-width
                                        :height root-height)))
                     exwm-workspace--list)))
    ;; Exclude areas occupied by struts.
    (dolist (struts exwm-workspace--struts)
      (let* ((edge (aref struts 0))
             (size (aref struts 1))
             (position (aref struts 2))
             (beg (and position (aref position 0)))
             (end (and position (aref position 1)))
             delta)
        (dolist (w workareas)
          (with-slots (x y width height) w
            (pcase edge
              ;; Left and top are always processed first.
              ('left
               (setq delta (- size x))
               (when (and (< 0 delta)
                          (< delta width)
                          (or (not position)
                              (< (max beg y)
                                 (min end (+ y height)))))
                 (cl-decf width delta)
                 (setf x size)))
              ('right
               (setq delta (- size (- root-width x width)))
               (when (and (< 0 delta)
                          (< delta width)
                          (or (not position)
                              (< (max beg y)
                                 (min end (+ y height)))))
                 (cl-decf width delta)))
              ('top
               (setq delta (- size y))
               (when (and (< 0 delta)
                          (< delta height)
                          (or (not position)
                              (< (max beg x)
                                 (min end (+ x width)))))
                 (cl-decf height delta)
                 (setf y size)))
              ('bottom
               (setq delta (- size (- root-height y height)))
               (when (and (< 0 delta)
                          (< delta height)
                          (or (not position)
                              (< (max beg x)
                                 (min end (+ x width)))))
                 (cl-decf height delta))))))))
    ;; Save the result.
    (setq exwm-workspace--workareas workareas)
    (xcb:flush exwm--connection))
  (exwm--log "%s" exwm-workspace--workareas)
  (run-hooks 'exwm-workspace--update-workareas-hook))

(defun exwm-workspace--update-offsets ()
  "Update `exwm-workspace--frame-y-offset'/`exwm-workspace--window-y-offset'."
  (exwm--log)
  (if (not (and exwm-workspace--list
                (or menu-bar-mode tool-bar-mode)))
      (setq exwm-workspace--frame-y-offset 0
            exwm-workspace--window-y-offset 0)
    (redisplay t)
    (let* ((frame (elt exwm-workspace--list 0))
           (edges (window-inside-absolute-pixel-edges (frame-first-window
                                                       frame))))
      (with-slots (y)
          (xcb:+request-unchecked+reply exwm--connection
              (make-instance 'xcb:GetGeometry
                             :drawable (frame-parameter frame
                                                        'exwm-container)))
        (with-slots ((y* y))
            (xcb:+request-unchecked+reply exwm--connection
                (make-instance 'xcb:GetGeometry
                               :drawable (frame-parameter frame
                                                          'exwm-outer-id)))
          (with-slots ((y** y))
              (xcb:+request-unchecked+reply exwm--connection
                  (make-instance 'xcb:GetGeometry
                                 :drawable (frame-parameter frame 'exwm-id)))
            (setq exwm-workspace--frame-y-offset (- y** y*)
                  exwm-workspace--window-y-offset (- (elt edges 1) y))))))))

(defun exwm-workspace--set-active (frame active)
  "Make frame FRAME active on its monitor.
ACTIVE indicates whether to set the frame active or inactive."
  (exwm--log "active=%s; frame=%s" active frame)
  (set-frame-parameter frame 'exwm-active active)
  (if active
      (exwm-workspace--set-fullscreen frame)
    (exwm--set-geometry (frame-parameter frame 'exwm-container) nil nil 1 1))
  (exwm-layout--refresh frame)
  (xcb:flush exwm--connection))

(defun exwm-workspace--active-p (frame)
  "Return non-nil if FRAME is active."
  (frame-parameter frame 'exwm-active))

(defun exwm-workspace--set-fullscreen (frame)
  "Make frame FRAME fullscreen according to `exwm-workspace--workareas'."
  (exwm--log "frame=%s" frame)
  (let ((id (frame-parameter frame 'exwm-outer-id))
        (container (frame-parameter frame 'exwm-container)))
    (with-slots (x y width height)
        (exwm-workspace--workarea frame)
      (exwm--log "x=%s; y=%s; w=%s; h=%s" x y width height)
      (when (and (eq frame exwm-workspace--current)
                 (exwm-workspace--minibuffer-own-frame-p))
        (exwm-workspace--resize-minibuffer-frame))
      (if (exwm-workspace--active-p frame)
          (exwm--set-geometry container x y width height)
        (exwm--set-geometry container x y 1 1))
      (exwm--set-geometry id nil nil width height)
      (xcb:flush exwm--connection)))
  ;; This is only used for workspace initialization.
  (when exwm-workspace--fullscreen-frame-count
    (cl-incf exwm-workspace--fullscreen-frame-count)))

(defun exwm-workspace--resize-minibuffer-frame ()
  "Resize minibuffer (and its container) to fit the size of workspace."
  (cl-assert (exwm-workspace--minibuffer-own-frame-p))
  (let ((workarea (exwm-workspace--workarea exwm-workspace-current-index))
        (container (frame-parameter exwm-workspace--minibuffer
                                    'exwm-container))
        y width)
    (setq y (if (eq exwm-workspace-minibuffer-position 'top)
                (- (slot-value workarea 'y)
                   exwm-workspace--attached-minibuffer-height)
              ;; Reset the frame size.
              (set-frame-height exwm-workspace--minibuffer 1)
              (redisplay)               ;FIXME.
              (+ (slot-value workarea 'y) (slot-value workarea 'height)
                 (- (frame-pixel-height exwm-workspace--minibuffer))
                 exwm-workspace--attached-minibuffer-height))
          width (slot-value workarea 'width))
    (xcb:+request exwm--connection
        (make-instance 'xcb:ConfigureWindow
                       :window container
                       :value-mask (logior xcb:ConfigWindow:X
                                           xcb:ConfigWindow:Y
                                           xcb:ConfigWindow:Width
                                           (if exwm-manage--desktop
                                               xcb:ConfigWindow:Sibling
                                             0)
                                           xcb:ConfigWindow:StackMode)
                       :x (slot-value workarea 'x)
                       :y y
                       :width width
                       :sibling exwm-manage--desktop
                       :stack-mode (if exwm-manage--desktop
                                       xcb:StackMode:Above
                                     xcb:StackMode:Below)))
    (xcb:+request exwm--connection
        (make-instance 'xcb:ConfigureWindow
                       :window (frame-parameter exwm-workspace--minibuffer
                                                'exwm-outer-id)
                       :value-mask xcb:ConfigWindow:Width
                       :width width))
    (exwm--log "y: %s, width: %s" y width)))

(defun exwm-workspace--switch-map-nth-prefix (&optional prefix-digits)
  "Allow selecting a workspace by number.

PREFIX-DIGITS is a list of the digits introduced so far."
  (interactive)
  (let* ((k (aref (substring (this-command-keys-vector) -1) 0))
         (d (- k ?0))
         ;; Convert prefix-digits to number.  For example, '(2 1) to 120.
         (o 1)
         (pn (apply #'+ (mapcar (lambda (x)
                                  (setq o (* 10 o))
                                  (* o x))
                                prefix-digits)))
         (n (+ pn d))
         prefix-length index-max index-length)
    (if (or (= n 0)
            (> n
               (setq index-max (1- (exwm-workspace--count))))
            (>= (setq prefix-length (length prefix-digits))
                (setq index-length (floor (log index-max 10))))
            ;; Check if it's still possible to do a match.
            (> (* n (expt 10 (- index-length prefix-length)))
               index-max))
        (exwm-workspace--switch-map-select-nth n)
      ;; Go ahead if there are enough digits to select any workspace.
      (set-transient-map
       (let ((map (make-sparse-keymap))
             (cmd (let ((digits (cons d prefix-digits)))
                    (lambda ()
                     (interactive)
                     (exwm-workspace--switch-map-nth-prefix digits)))))
         (dotimes (i 10)
           (keymap-set map (int-to-string i) cmd))
         ;; Accept
         (keymap-set map "<return>"
           (lambda ()
             (interactive)
             (exwm-workspace--switch-map-select-nth n)))
         map)))))
(put #'exwm-workspace--switch-map-nth-prefix 'completion-predicate #'ignore) ;; Move to declare in Emacs 28

(defun exwm-workspace--switch-map-select-nth (n)
  "Select Nth workspace."
  (interactive)
  (goto-history-element (1+ n))
  (exit-minibuffer))
(put #'exwm-workspace--switch-map-select-nth 'completion-predicate #'ignore) ;; Move to declare in Emacs 28

(defun exwm-workspace-switch (frame-or-index &optional force)
  "Switch to workspace FRAME-OR-INDEX (0-based).

Query for the index if not specified when called interactively.  Passing a
workspace frame as the first option or making use of the rest options are
for internal use only.

When FORCE is true, allow switching to current workspace."
  (interactive
   (list
    (cond
     ((null current-prefix-arg)
      (unless (and (derived-mode-p 'exwm-mode)
                   ;; The prompt is invisible in fullscreen mode.
                   (exwm-layout--fullscreen-p))
        (let ((exwm-workspace--prompt-add-allowed t)
              (exwm-workspace--prompt-delete-allowed t))
          (exwm-workspace--prompt-for-workspace "Switch to [+/-]: "))))
     ((and (integerp current-prefix-arg)
           (<= 0 current-prefix-arg (exwm-workspace--count)))
      current-prefix-arg)
     (t 0))))
  (exwm--log)
  (let* ((frame (exwm-workspace--workspace-from-frame-or-index frame-or-index))
         (old-frame exwm-workspace--current)
         (index (exwm-workspace--position frame))
         (window (frame-parameter frame 'exwm-selected-window)))
    (when (or force (not (eq frame exwm-workspace--current)))
      (unless (window-live-p window)
        (setq window (frame-selected-window frame)))
    (when (and (not (eq frame old-frame))
               (frame-live-p old-frame))
      (with-selected-frame old-frame
        (funcall exwm-workspace--original-handle-focus-out
                 (list 'focus-out frame))))
      ;; Raise this frame.
      (xcb:+request exwm--connection
          (make-instance 'xcb:ConfigureWindow
                         :window (frame-parameter frame 'exwm-container)
                         :value-mask (logior xcb:ConfigWindow:Sibling
                                             xcb:ConfigWindow:StackMode)
                         :sibling exwm--guide-window
                         :stack-mode xcb:StackMode:Below))
      (setq exwm-workspace--current frame
            exwm-workspace-current-index index)
      (unless (exwm-workspace--workspace-p (selected-frame))
        ;; Save the floating frame window selected on the previous workspace.
        (set-frame-parameter (buffer-local-value 'exwm--frame (window-buffer))
                             'exwm-selected-window (selected-window)))
      ;; Show/Hide X windows.
      (let ((monitor-old (frame-parameter old-frame 'exwm-randr-monitor))
            (monitor-new (frame-parameter frame 'exwm-randr-monitor))
            (active-old (exwm-workspace--active-p old-frame))
            (active-new (exwm-workspace--active-p frame))
            workspaces-to-hide)
        (cond
         ((not active-old)
          (exwm-workspace--set-active frame t))
         ((equal monitor-old monitor-new)
          (exwm-workspace--set-active frame t)
          (unless (eq frame old-frame)
            (exwm-workspace--set-active old-frame nil)
            (setq workspaces-to-hide (list old-frame))))
         (active-new)
         (t
          (dolist (w exwm-workspace--list)
            (when (and (exwm-workspace--active-p w)
                       (equal monitor-new
                              (frame-parameter w 'exwm-randr-monitor)))
              (exwm-workspace--set-active w nil)
              (setq workspaces-to-hide (append workspaces-to-hide (list w)))))
          (exwm-workspace--set-active frame t)))
        (dolist (i exwm--id-buffer-alist)
          (with-current-buffer (cdr i)
            (if (memq exwm--frame workspaces-to-hide)
                (exwm-layout--hide exwm--id)
              (when (eq frame exwm--frame)
                (let ((window (get-buffer-window nil t)))
                  (when window
                    (exwm-layout--show exwm--id window))))))))
      (select-window window)
      (x-focus-frame (window-frame window)) ;The real input focus.
      (set-frame-parameter frame 'exwm-selected-window nil)
      (if (exwm-workspace--minibuffer-own-frame-p)
          ;; Resize the minibuffer frame.
          (exwm-workspace--resize-minibuffer-frame)
        ;; Set a default minibuffer frame.
        (setq default-minibuffer-frame frame))
      ;; Hide windows in other workspaces by preprending a space
      (unless exwm-workspace-show-all-buffers
        (dolist (i exwm--id-buffer-alist)
          (with-current-buffer (cdr i)
            (let ((name (replace-regexp-in-string "^\\s-*" ""
                                                  (buffer-name))))
              (exwm-workspace-rename-buffer (if (eq frame exwm--frame)
                                                name
                                              (concat " " name)))))))
      ;; Update demands attention flag
      (set-frame-parameter frame 'exwm-urgency nil)
      ;; Update switch workspace history
      (setq exwm-workspace--switch-history-outdated t)
      ;; Set _NET_CURRENT_DESKTOP
      (xcb:+request exwm--connection
          (make-instance 'xcb:ewmh:set-_NET_CURRENT_DESKTOP
                         :window exwm--root :data index))
      (xcb:flush exwm--connection))
    (when exwm-workspace-warp-cursor
      (with-slots (win-x win-y)
          (xcb:+request-unchecked+reply exwm--connection
              (make-instance 'xcb:QueryPointer
                             :window (frame-parameter frame
                                                      'exwm-outer-id)))
        (when (or (< win-x 0)
                  (< win-y 0)
                  (> win-x (frame-pixel-width frame))
                  (> win-y (frame-pixel-height frame)))
          (xcb:+request exwm--connection
              (make-instance 'xcb:WarpPointer
                             :src-window xcb:Window:None
                             :dst-window (frame-parameter frame
                                                          'exwm-outer-id)
                             :src-x 0
                             :src-y 0
                             :src-width 0
                             :src-height 0
                             :dst-x (/ (frame-pixel-width frame) 2)
                             :dst-y (/ (frame-pixel-height frame) 2)))
          (xcb:flush exwm--connection))))
    (funcall exwm-workspace--original-handle-focus-in (list 'focus-in frame))
    (run-hooks 'exwm-workspace-switch-hook)))

(defun exwm-workspace-switch-create (frame-or-index)
  "Switch to workspace FRAME-OR-INDEX creating it first non-existent.

Passing a workspace frame as the first option is for internal use only."
  (interactive
   (list
    (cond
     ((integerp current-prefix-arg)
      current-prefix-arg)
     (t 0))))
  (unless frame-or-index
    (setq frame-or-index 0))
  (exwm--log "%s" frame-or-index)
  (if (or (framep frame-or-index)
          (< frame-or-index (exwm-workspace--count)))
      (exwm-workspace-switch frame-or-index)
    (let ((count (1+ (- frame-or-index (exwm-workspace--count))))
          (exwm-workspace--create-silently t))
      (when (< exwm-workspace-switch-create-limit count)
        (user-error "Cannot implicitly create more than %d workspaces at a time, customize `exwm-workspace-switch-create-limit' to change this limit" exwm-workspace-switch-create-limit))
      (dotimes (_ count) (make-frame))
      (run-hooks 'exwm-workspace-list-change-hook))
    (exwm-workspace-switch frame-or-index)))

(defun exwm-workspace-swap (workspace1 workspace2)
  "Interchange position of WORKSPACE1 with that of WORKSPACE2."
  (interactive
   (unless (and (derived-mode-p 'exwm-mode)
                ;; The prompt is invisible in fullscreen mode.
                (exwm-layout--fullscreen-p))
     (let (w1 w2)
       (let ((exwm-workspace--prompt-add-allowed t)
             (exwm-workspace--prompt-delete-allowed t))
         (setq w1 (exwm-workspace--prompt-for-workspace
                   "Pick a workspace [+/-]: ")))
       (setq w2 (exwm-workspace--prompt-for-workspace
                 (format "Swap workspace %d with: "
                         (exwm-workspace--position w1))))
       (list w1 w2))))
  (exwm--log)
  (let ((pos1 (exwm-workspace--position workspace1))
        (pos2 (exwm-workspace--position workspace2)))
    (if (or (not pos1) (not pos2) (= pos1 pos2))
        (user-error "[EXWM] Cannot swap %s and %s" workspace1 workspace2)
      (setf (elt exwm-workspace--list pos1) workspace2)
      (setf (elt exwm-workspace--list pos2) workspace1)
      ;; Update the _NET_WM_DESKTOP property of each X window affected.
      (dolist (pair exwm--id-buffer-alist)
        (when (memq (buffer-local-value 'exwm--frame (cdr pair))
                    (list workspace1 workspace2))
          (exwm-workspace--set-desktop (car pair))))
      (xcb:flush exwm--connection)
      (when (memq exwm-workspace--current (list workspace1 workspace2))
        ;; With the current workspace involved, lots of stuffs need refresh.
        (set-frame-parameter exwm-workspace--current 'exwm-selected-window
                             (selected-window))
        (exwm-workspace-switch exwm-workspace--current t))
      (run-hooks 'exwm-workspace-list-change-hook))))

(defun exwm-workspace-move (workspace nth)
  "Move WORKSPACE to the NTH position.

When called interactively, prompt for a workspace and move current one just
before it."
  (interactive
   (cond
    ((null current-prefix-arg)
     (unless (and (derived-mode-p 'exwm-mode)
                  ;; The prompt is invisible in fullscreen mode.
                  (exwm-layout--fullscreen-p))
       (list exwm-workspace--current
             (exwm-workspace--position
              (exwm-workspace--prompt-for-workspace "Move workspace to: ")))))
    ((and (integerp current-prefix-arg)
          (<= 0 current-prefix-arg (exwm-workspace--count)))
     (list exwm-workspace--current current-prefix-arg))
    (t (list exwm-workspace--current 0))))
  (exwm--log)
  (let ((pos (exwm-workspace--position workspace))
        flag start end index)
    (if (= nth pos)
        (user-error "[EXWM] Cannot move to same position")
      ;; Set if the current workspace is involved.
      (setq flag (or (eq workspace exwm-workspace--current)
                     (eq (elt exwm-workspace--list nth)
                         exwm-workspace--current)))
      ;; Do the move.
      (with-no-warnings                 ;For Emacs 24.
        (pop (nthcdr pos exwm-workspace--list)))
      (push workspace (nthcdr nth exwm-workspace--list))
      ;; Update the _NET_WM_DESKTOP property of each X window affected.
      (setq start (min pos nth)
            end (max pos nth))
      (dolist (pair exwm--id-buffer-alist)
        (setq index (exwm-workspace--position
                     (buffer-local-value 'exwm--frame (cdr pair))))
        (unless (or (< index start) (> index end))
          (exwm-workspace--set-desktop (car pair))))
      (when flag
        ;; With the current workspace involved, lots of stuffs need refresh.
        (set-frame-parameter exwm-workspace--current 'exwm-selected-window
                             (selected-window))
        (exwm-workspace-switch exwm-workspace--current t))
      (run-hooks 'exwm-workspace-list-change-hook))))

(defun exwm-workspace-add (&optional index)
  "Add a workspace as the INDEX-th workspace, or the last one if INDEX is nil.

INDEX must not exceed the current number of workspaces."
  (interactive)
  (exwm--log "%s" index)
  (if (and index
           ;; No need to move if it's the last one.
           (< index (exwm-workspace--count)))
      (exwm-workspace-move (make-frame) index)
    (make-frame)))

(defun exwm-workspace-delete (&optional frame-or-index)
  "Delete the workspace FRAME-OR-INDEX."
  (interactive)
  (exwm--log "%s" frame-or-index)
  (when (< 1 (exwm-workspace--count))
    (let ((frame (if frame-or-index
                     (exwm-workspace--workspace-from-frame-or-index
                      frame-or-index)
                   exwm-workspace--current)))
      ;; Transfer over any surrogate minibuffers before trying to delete the workspace.
      (let ((minibuf (minibuffer-window frame))
            (newminibuf (minibuffer-window (exwm-workspace--get-next-workspace frame))))
        (dolist (f (filtered-frame-list (lambda (f) (eq (frame-parameter f 'minibuffer) minibuf))))
          (set-frame-parameter f 'minibuffer newminibuf)))
      (delete-frame frame))))

(defun exwm-workspace--set-desktop (id)
  "Set _NET_WM_DESKTOP for X window ID."
  (exwm--log "#x%x" id)
  (with-current-buffer (exwm--id->buffer id)
    (let ((desktop (exwm-workspace--position exwm--frame)))
      (setq exwm--desktop desktop)
      (xcb:+request exwm--connection
          (make-instance 'xcb:ewmh:set-_NET_WM_DESKTOP
                         :window id
                         :data desktop)))))

(cl-defun exwm-workspace-move-window (frame-or-index &optional id)
  "Move window ID to workspace FRAME-OR-INDEX."
  (interactive (list
                (cond
                 ((null current-prefix-arg)
                  (let ((exwm-workspace--prompt-add-allowed t)
                        (exwm-workspace--prompt-delete-allowed t))
                    (exwm-workspace--prompt-for-workspace "Move to [+/-]: ")))
                 ((and (integerp current-prefix-arg)
                       (<= 0 current-prefix-arg (exwm-workspace--count)))
                  current-prefix-arg)
                 (t 0))))
  (let ((frame (exwm-workspace--workspace-from-frame-or-index frame-or-index))
        old-frame container)
    (unless id (setq id (exwm--buffer->id (window-buffer))))
    (unless id
      (cl-return-from exwm-workspace-move-window))
    (exwm--log "Moving #x%x to %s" id frame-or-index)
    (with-current-buffer (exwm--id->buffer id)
      (unless (eq exwm--frame frame)
        (unless exwm-workspace-show-all-buffers
          (let ((name (replace-regexp-in-string "^\\s-*" "" (buffer-name))))
            (exwm-workspace-rename-buffer
             (if (eq frame exwm-workspace--current)
                 name
               (concat " " name)))))
        (setq old-frame exwm--frame
              exwm--frame frame)
        (if (not exwm--floating-frame)
            ;; Tiling.
            (if (get-buffer-window nil frame)
                (when (eq frame exwm-workspace--current)
                  (exwm-layout--refresh frame))
              (set-window-buffer (get-buffer-window nil t)
                                 (other-buffer nil t))
              (unless (eq frame exwm-workspace--current)
                ;; Clear the 'exwm-selected-window' frame parameter.
                (set-frame-parameter frame 'exwm-selected-window nil))
              (set-window-buffer (frame-selected-window frame)
                                 (exwm--id->buffer id))
              (if (eq frame exwm-workspace--current)
                  (select-window (frame-selected-window frame))
                (unless (exwm-workspace--active-p frame)
                  (exwm-layout--hide id))))
          ;; Floating.
          (setq container (frame-parameter exwm--floating-frame
                                           'exwm-container))
          (unless (equal (frame-parameter old-frame 'exwm-randr-monitor)
                         (frame-parameter frame 'exwm-randr-monitor))
            (with-slots (x y)
                (xcb:+request-unchecked+reply exwm--connection
                    (make-instance 'xcb:GetGeometry
                                   :drawable container))
              (with-slots ((x1 x)
                           (y1 y))
                  (exwm-workspace--get-geometry old-frame)
                (with-slots ((x2 x)
                             (y2 y))
                    (exwm-workspace--get-geometry frame)
                  (setq x (+ x (- x2 x1))
                        y (+ y (- y2 y1)))))
              (exwm--set-geometry id x y nil nil)
              (exwm--set-geometry container x y nil nil)))
          (if (exwm-workspace--minibuffer-own-frame-p)
              (if (eq frame exwm-workspace--current)
                  (select-window (frame-root-window exwm--floating-frame))
                (select-window (frame-selected-window exwm-workspace--current))
                (unless (exwm-workspace--active-p frame)
                  (exwm-layout--hide id)))
            ;; The frame needs to be recreated since it won't use the
            ;; minibuffer on the new workspace.
            ;; The code is mostly copied from `exwm-floating--set-floating'.
            (let* ((old-frame exwm--floating-frame)
                   (new-frame
                    (with-current-buffer
                        (or (get-buffer "*scratch*")
                            (progn
                              (set-buffer-major-mode
                               (get-buffer-create "*scratch*"))
                              (get-buffer "*scratch*")))
                      (make-frame
                       `((minibuffer . ,(minibuffer-window frame))
                         (left . ,(* window-min-width -100))
                         (top . ,(* window-min-height -100))
                         (width . ,window-min-width)
                         (height . ,window-min-height)
                         (unsplittable . t)))))
                   (outer-id (string-to-number
                              (frame-parameter new-frame
                                               'outer-window-id)))
                   (window-id (string-to-number
                               (frame-parameter new-frame 'window-id)))
                   (window (frame-root-window new-frame)))
              (set-frame-parameter new-frame 'exwm-outer-id outer-id)
              (set-frame-parameter new-frame 'exwm-id window-id)
              (set-frame-parameter new-frame 'exwm-container container)
              (make-frame-invisible new-frame)
              (set-frame-size new-frame
                              (frame-pixel-width old-frame)
                              (frame-pixel-height old-frame)
                              t)
              (xcb:+request exwm--connection
                  (make-instance 'xcb:ReparentWindow
                                 :window outer-id
                                 :parent container
                                 :x 0 :y 0))
              (xcb:flush exwm--connection)
              (with-current-buffer (exwm--id->buffer id)
                (setq window-size-fixed nil
                      exwm--floating-frame new-frame)
                (set-window-dedicated-p (frame-root-window old-frame) nil)
                (remove-hook 'window-configuration-change-hook
                             #'exwm-layout--refresh)
                (set-window-buffer window (current-buffer))
                (add-hook 'window-configuration-change-hook
                          #'exwm-layout--refresh)
                (set-window-dedicated-p window t))
              ;; Select a tiling window and delete the old frame.
              (select-window (frame-selected-window exwm-workspace--current))
              (delete-frame old-frame)
              ;; The rest is the same.
              (make-frame-visible new-frame)
              (exwm--set-geometry outer-id 0 0 nil nil)
              (xcb:flush exwm--connection)
              (redisplay)
              (if (eq frame exwm-workspace--current)
                  (with-current-buffer (exwm--id->buffer id)
                    (select-window (frame-root-window exwm--floating-frame)))
                (unless (exwm-workspace--active-p frame)
                  (exwm-layout--hide id)))))
          ;; Update the 'exwm-selected-window' frame parameter.
          (unless (eq frame exwm-workspace--current)
            (with-current-buffer (exwm--id->buffer id)
              (set-frame-parameter frame 'exwm-selected-window
                                   (frame-root-window
                                    exwm--floating-frame)))))
        ;; Set _NET_WM_DESKTOP.
        (exwm-workspace--set-desktop id)
        (xcb:flush exwm--connection)))
    (setq exwm-workspace--switch-history-outdated t)))

(defun exwm-workspace-switch-to-buffer (buffer-or-name)
  "Make selected window display BUFFER-OR-NAME."
  (interactive
   (let ((inhibit-quit t))
     ;; Show all buffers
     (unless exwm-workspace-show-all-buffers
       (dolist (pair exwm--id-buffer-alist)
         (with-current-buffer (cdr pair)
           (when (= ?\s (aref (buffer-name) 0))
             (let ((buffer-list-update-hook
                    (remq #'exwm-input--on-buffer-list-update
                          buffer-list-update-hook)))
               (rename-buffer (substring (buffer-name) 1)))))))
     (prog1
         (with-local-quit
           (list (get-buffer (read-buffer-to-switch "Switch to buffer: "))))
       ;; Hide buffers on other workspaces
       (unless exwm-workspace-show-all-buffers
         (dolist (pair exwm--id-buffer-alist)
           (with-current-buffer (cdr pair)
             (unless (or (eq exwm--frame exwm-workspace--current)
                         (= ?\s (aref (buffer-name) 0)))
               (let ((buffer-list-update-hook
                      (remq #'exwm-input--on-buffer-list-update
                            buffer-list-update-hook)))
                 (rename-buffer (concat " " (buffer-name)))))))))))
  (exwm--log)
  (when buffer-or-name
    (with-current-buffer buffer-or-name
      (if (derived-mode-p 'exwm-mode)
          ;; EXWM buffer.
          (if (eq exwm--frame exwm-workspace--current)
              ;; On the current workspace.
              (if (not exwm--floating-frame)
                  (switch-to-buffer buffer-or-name)
                ;; Select the floating frame.
                (select-frame-set-input-focus exwm--floating-frame)
                (select-window (frame-root-window exwm--floating-frame)))
            ;; On another workspace.
            (if exwm-layout-show-all-buffers
                (exwm-workspace-move-window exwm-workspace--current
                                            exwm--id)
              (let ((window (get-buffer-window buffer-or-name exwm--frame)))
                (if window
                    (set-frame-parameter exwm--frame
                                         'exwm-selected-window window)
                  (set-window-buffer (frame-selected-window exwm--frame)
                                     buffer-or-name)))
              (exwm-workspace-switch exwm--frame)))
        ;; Ordinary buffer.
        (switch-to-buffer buffer-or-name)))))

(defun exwm-workspace-rename-buffer (newname)
  "Rename current buffer to NEWNAME."
  (let ((hidden (= ?\s (aref newname 0)))
        (basename (replace-regexp-in-string "<[0-9]+>$" "" newname))
        (counter 1)
        tmp)
    (when hidden (setq basename (substring basename 1)))
    (setq newname basename)
    (while (and (setq tmp (or (get-buffer newname)
                              (get-buffer (concat " " newname))))
                (not (eq tmp (current-buffer))))
      (setq newname (format "%s<%d>" basename (cl-incf counter))))
    (let ((buffer-list-update-hook
           (remq #'exwm-input--on-buffer-list-update
                 buffer-list-update-hook)))
      (rename-buffer (concat (and hidden " ") newname)))))

(defun exwm-workspace--x-create-frame (orig-x-create-frame params)
  "Set override-redirect on the frame created by `x-create-frame'.
ORIG-X-CREATE-FRAME is the advised function `x-create-frame'.
PARAMS are the original arguments."
  (exwm--log)
  (let ((frame (funcall orig-x-create-frame params)))
    (xcb:+request exwm--connection
        (make-instance 'xcb:ChangeWindowAttributes
                       :window (string-to-number
                                (frame-parameter frame 'outer-window-id))
                       :value-mask xcb:CW:OverrideRedirect
                       :override-redirect 1))
    (xcb:flush exwm--connection)
    frame))

(defsubst exwm-workspace--minibuffer-attached-p ()
  "Return non-nil if the minibuffer is attached.

Please check `exwm-workspace--minibuffer-own-frame-p' first."
  (assq (frame-parameter exwm-workspace--minibuffer 'exwm-container)
        exwm-workspace--id-struts-alist))

(defun exwm-workspace-attach-minibuffer ()
  "Attach the minibuffer making it always visible."
  (interactive)
  (exwm--log)
  (when (and (exwm-workspace--minibuffer-own-frame-p)
             (not (exwm-workspace--minibuffer-attached-p)))
    ;; Reset the frame size.
    (set-frame-height exwm-workspace--minibuffer 1)
    (redisplay)                       ;FIXME.
    (setq exwm-workspace--attached-minibuffer-height
          (frame-pixel-height exwm-workspace--minibuffer))
    (exwm-workspace--show-minibuffer)
    (let ((container (frame-parameter exwm-workspace--minibuffer
                                      'exwm-container)))
      (push (cons container
                  (if (eq exwm-workspace-minibuffer-position 'top)
                      (vector 0 0 exwm-workspace--attached-minibuffer-height 0)
                    (vector 0 0 0 exwm-workspace--attached-minibuffer-height)))
            exwm-workspace--id-struts-alist)
      (exwm-workspace--update-struts)
      (exwm-workspace--update-workareas)
      (dolist (f exwm-workspace--list)
        (exwm-workspace--set-fullscreen f)))))

(defun exwm-workspace-detach-minibuffer ()
  "Detach the minibuffer so that it automatically hides."
  (interactive)
  (exwm--log)
  (when (and (exwm-workspace--minibuffer-own-frame-p)
             (exwm-workspace--minibuffer-attached-p))
    (setq exwm-workspace--attached-minibuffer-height 0)
    (let ((container (frame-parameter exwm-workspace--minibuffer
                                      'exwm-container)))
      (setq exwm-workspace--id-struts-alist
            (assq-delete-all container exwm-workspace--id-struts-alist))
      (exwm-workspace--update-struts)
      (exwm-workspace--update-workareas)
      (dolist (f exwm-workspace--list)
        (exwm-workspace--set-fullscreen f))
      (exwm-workspace--hide-minibuffer))))

(defun exwm-workspace-toggle-minibuffer ()
  "Attach the minibuffer if it's detached, or detach it if it's attached."
  (interactive)
  (exwm--log)
  (when (exwm-workspace--minibuffer-own-frame-p)
    (if (exwm-workspace--minibuffer-attached-p)
        (exwm-workspace-detach-minibuffer)
      (exwm-workspace-attach-minibuffer))))

(defun exwm-workspace--update-minibuffer-height (&optional echo-area)
  "Update the minibuffer frame height.
When ECHO-AREA is non-nil, take the size of the echo area into
account when calculating the height."
  (when (exwm--terminal-p)
    (let ((height
           (with-current-buffer
               (window-buffer (minibuffer-window exwm-workspace--minibuffer))
             (max 1
                  (if echo-area
                      (let ((width (frame-width exwm-workspace--minibuffer))
                            (result 0))
                        (mapc (lambda (i)
                                (setq result
                                      (+ result
                                         (ceiling (1+ (length i)) width))))
                              (split-string (or (current-message) "") "\n"))
                        result)
                    (count-screen-lines))))))
      (when (and (integerp max-mini-window-height)
                 (> height max-mini-window-height))
        (setq height max-mini-window-height))
      (exwm--log "%s" height)
      (set-frame-height exwm-workspace--minibuffer height))))

(defun exwm-workspace--on-ConfigureNotify (data _synthetic)
  "Adjust the container to fit the minibuffer frame.
DATA contains unmarshalled ConfigureNotify event data."
  (let ((obj (make-instance 'xcb:ConfigureNotify)) y)
    (xcb:unmarshal obj data)
    (with-slots (window height) obj
      (when (eq (frame-parameter exwm-workspace--minibuffer 'exwm-outer-id)
                window)
        (exwm--log)
        (when (and (floatp max-mini-window-height)
                   (> height (* max-mini-window-height
                                (exwm-workspace--current-height))))
          (setq height (floor
                        (* max-mini-window-height
                           (exwm-workspace--current-height))))
          (xcb:+request exwm--connection
              (make-instance 'xcb:ConfigureWindow
                             :window window
                             :value-mask xcb:ConfigWindow:Height
                             :height height)))
        (when (/= (exwm-workspace--count) (length exwm-workspace--workareas))
          ;; There is a chance the workareas are not updated timely.
          (exwm-workspace--update-workareas))
        (with-slots ((y* y) (height* height))
            (exwm-workspace--workarea exwm-workspace-current-index)
          (setq y (if (eq exwm-workspace-minibuffer-position 'top)
                      (- y*
                         exwm-workspace--attached-minibuffer-height)
                    (+ y* height* (- height)
                       exwm-workspace--attached-minibuffer-height))))
        (xcb:+request exwm--connection
            (make-instance 'xcb:ConfigureWindow
                           :window (frame-parameter exwm-workspace--minibuffer
                                                    'exwm-container)
                           :value-mask (logior xcb:ConfigWindow:Y
                                               xcb:ConfigWindow:Height)
                           :y y
                           :height height))
        (xcb:flush exwm--connection)))))

(defun exwm-workspace--display-buffer (buffer alist)
  "Display BUFFER as if the current workspace were selected.
ALIST is an action alist, as accepted by function `display-buffer'."
  ;; Only when the floating minibuffer frame is selected.
  ;; This also protect this functions from being recursively called.
  (when (eq (selected-frame) exwm-workspace--minibuffer)
    (with-selected-frame exwm-workspace--current
      (display-buffer buffer alist))))

(defun exwm-workspace--show-minibuffer ()
  "Show the minibuffer frame."
  (exwm--log)
  ;; Cancel pending timer.
  (when exwm-workspace--display-echo-area-timer
    (cancel-timer exwm-workspace--display-echo-area-timer)
    (setq exwm-workspace--display-echo-area-timer nil))
  ;; Show the minibuffer frame.
  (unless (exwm-workspace--minibuffer-attached-p)
    (exwm--set-geometry (frame-parameter exwm-workspace--minibuffer
                                         'exwm-container)
                        nil nil
                        (frame-pixel-width exwm-workspace--minibuffer)
                        (frame-pixel-height exwm-workspace--minibuffer)))
  (xcb:+request exwm--connection
      (make-instance 'xcb:ConfigureWindow
                     :window (frame-parameter exwm-workspace--minibuffer
                                              'exwm-container)
                     :value-mask xcb:ConfigWindow:StackMode
                     :stack-mode xcb:StackMode:Above))
  (xcb:flush exwm--connection))

(defun exwm-workspace--hide-minibuffer ()
  "Hide the minibuffer frame."
  (exwm--log)
  ;; Hide the minibuffer frame.
  (if (exwm-workspace--minibuffer-attached-p)
      (xcb:+request exwm--connection
          (make-instance 'xcb:ConfigureWindow
                         :window (frame-parameter exwm-workspace--minibuffer
                                                  'exwm-container)
                         :value-mask (logior (if exwm-manage--desktop
                                                 xcb:ConfigWindow:Sibling
                                               0)
                                             xcb:ConfigWindow:StackMode)
                         :sibling exwm-manage--desktop
                         :stack-mode (if exwm-manage--desktop
                                         xcb:StackMode:Above
                                       xcb:StackMode:Below)))
    (exwm--set-geometry (frame-parameter exwm-workspace--minibuffer
                                         'exwm-container)
                        nil nil 1 1))
  (xcb:flush exwm--connection))

(defun exwm-workspace--on-minibuffer-setup ()
  "Run in `minibuffer-setup-hook' to show the minibuffer and its container."
  (exwm--log)
  (when (and (= 1 (minibuffer-depth))
             (exwm--terminal-p))
    (add-hook 'post-command-hook #'exwm-workspace--update-minibuffer-height)
    (exwm-workspace--show-minibuffer))
  ;; FIXME: This is a temporary fix for the *Completions* buffer not
  ;;        being correctly fitted by its displaying window.  As with
  ;;        `exwm-workspace--display-buffer', the problem is caused by
  ;;        the fact that the minibuffer (rather than the workspace)
  ;;        frame is the 'selected frame'.  `get-buffer-window' will
  ;;        fail to retrieve the correct window.  It's likely there are
  ;;        other related issues.
  ;; This is not required by Emacs 24.
  (let ((window (get-buffer-window "*Completions*" exwm-workspace--current)))
    (when window
      (fit-window-to-buffer window)
      (window-preserve-size window))))

(defun exwm-workspace--on-minibuffer-exit ()
  "Run in `minibuffer-exit-hook' to hide the minibuffer container."
  (exwm--log)
  (when (and (= 1 (minibuffer-depth))
             (exwm--terminal-p))
    (remove-hook 'post-command-hook #'exwm-workspace--update-minibuffer-height)
    (exwm-workspace--hide-minibuffer)))

(defun exwm-workspace--on-echo-area-dirty ()
  "Run when new message arrives to show the echo area and its container."
  (when (and (not (active-minibuffer-window))
             (or (current-message)
                 cursor-in-echo-area)
             (exwm--terminal-p))
    (exwm-workspace--update-minibuffer-height t)
    (exwm-workspace--show-minibuffer)
    (unless (or (not exwm-workspace-display-echo-area-timeout)
                real-this-command ;e.g. read-event
                input-method-use-echo-area)
      (setq exwm-workspace--display-echo-area-timer
            (run-with-timer exwm-workspace-display-echo-area-timeout nil
                            #'exwm-workspace--echo-area-maybe-clear)))))

(defun exwm-workspace--echo-area-maybe-clear ()
  "Eventually clear the echo area container."
  (exwm--log)
  (if (not (current-message))
      (exwm-workspace--on-echo-area-clear)
    ;; Reschedule.
    (cancel-timer exwm-workspace--display-echo-area-timer)
    (setq exwm-workspace--display-echo-area-timer
          (run-with-timer exwm-workspace-display-echo-area-timeout nil
                          #'exwm-workspace--echo-area-maybe-clear))))

(defun exwm-workspace--on-echo-area-clear ()
  "Run in `echo-area-clear-hook' to hide echo area container."
  (when (exwm--terminal-p)
    (unless (active-minibuffer-window)
      (exwm-workspace--hide-minibuffer))
    (when exwm-workspace--display-echo-area-timer
      (cancel-timer exwm-workspace--display-echo-area-timer)
      (setq exwm-workspace--display-echo-area-timer nil))))

(defun exwm-workspace--set-desktop-geometry ()
  "Set _NET_DESKTOP_GEOMETRY."
  (exwm--log)
  ;; We don't support large desktop so it's the same with screen size.
  (xcb:+request exwm--connection
      (make-instance 'xcb:ewmh:set-_NET_DESKTOP_GEOMETRY
                     :window exwm--root
                     :width (x-display-pixel-width)
                     :height (x-display-pixel-height))))

(defun exwm-workspace--add-frame-as-workspace (frame)
  "Configure frame FRAME to be treated as a workspace."
  (exwm--log "%s" frame)
  (setq exwm-workspace--list (nconc exwm-workspace--list (list frame)))
  (let ((outer-id (string-to-number (frame-parameter frame
                                                     'outer-window-id)))
        (window-id (string-to-number (frame-parameter frame 'window-id)))
        (container (xcb:generate-id exwm--connection))
        frame-colormap frame-visual frame-depth)
    ;; Save window IDs
    (set-frame-parameter frame 'exwm-outer-id outer-id)
    (set-frame-parameter frame 'exwm-id window-id)
    (set-frame-parameter frame 'exwm-container container)
    ;; Copy RandR frame parameters from the first workspace to
    ;; prevent potential problems.  The values do not matter here as
    ;; they'll be updated by the RandR module later.
    (let ((w (car exwm-workspace--list)))
      (dolist (param '(exwm-randr-monitor
                       exwm-geometry))
        (set-frame-parameter frame param (frame-parameter w param))))
    ;; Support transparency on the container X window when the Emacs frame
    ;; does.  Note that in addition to setting the visual, colormap and depth
    ;; we must also reset the `:border-pixmap', as its default value is
    ;; relative to the parent window, which might have a different depth.
    (let* ((vdc (exwm--get-visual-depth-colormap exwm--connection outer-id)))
      (setq frame-visual (car vdc))
      (setq frame-depth (cadr vdc))
      (setq frame-colormap (caddr vdc)))
    (xcb:+request exwm--connection
        (make-instance 'xcb:CreateWindow
                       :depth frame-depth
                       :wid container
                       :parent exwm--root
                       :x -1
                       :y -1
                       :width 1
                       :height 1
                       :border-width 0
                       :class xcb:WindowClass:InputOutput
                       :visual frame-visual
                       :value-mask (logior xcb:CW:BackPixmap
                                           xcb:CW:BorderPixel
                                           xcb:CW:Colormap
                                           xcb:CW:OverrideRedirect)
                       :background-pixmap xcb:BackPixmap:None
                       :border-pixel 0
                       :colormap frame-colormap
                       :override-redirect 1))
    (xcb:+request exwm--connection
        (make-instance 'xcb:ConfigureWindow
                       :window container
                       :value-mask xcb:ConfigWindow:StackMode
                       :stack-mode xcb:StackMode:Below))
    (xcb:+request exwm--connection
        (make-instance 'xcb:ewmh:set-_NET_WM_NAME
                       :window container
                       :data
                       (format "EXWM workspace %d frame container"
                               (exwm-workspace--position frame))))
    (xcb:+request exwm--connection
        (make-instance 'xcb:ReparentWindow
                       :window outer-id :parent container :x 0 :y 0))
    (xcb:+request exwm--connection
        (make-instance 'xcb:icccm:set-WM_STATE
                       :window outer-id
                       :state xcb:icccm:WM_STATE:NormalState
                       :icon xcb:Window:None))
    (xcb:+request exwm--connection
        (make-instance 'xcb:MapWindow :window container)))
  (xcb:flush exwm--connection)
  ;; Delay making the workspace fullscreen until Emacs becomes idle
  (exwm--defer 0 #'exwm-workspace--fullscreen-workspace frame)
  ;; Update EWMH properties.
  (exwm-workspace--update-ewmh-props)
  (if exwm-workspace--create-silently
      (setq exwm-workspace--switch-history-outdated t)
    (let ((original-index exwm-workspace-current-index))
      (exwm-workspace-switch frame t)
      (message "Created %s as workspace %d; switched from %d"
               frame exwm-workspace-current-index original-index))
    (run-hooks 'exwm-workspace-list-change-hook)))

(defun exwm-workspace--get-next-workspace (frame)
  "Return the next workspace if workspace FRAME were removed.
Return nil if FRAME is the only workspace."
  (let* ((index (exwm-workspace--position frame))
         (lastp (= index (1- (exwm-workspace--count))))
         (nextw (elt exwm-workspace--list (+ index (if lastp -1 +1)))))
    (unless (eq frame nextw)
      nextw)))

(defun exwm-workspace--remove-frame-as-workspace (frame &optional quit)
  "Stop treating FRAME as a workspace.
When QUIT is non-nil cleanup avoid communicating with the X server."
  ;; TODO: restore all frame parameters (e.g. exwm-workspace, buffer-predicate,
  ;; etc)
  (exwm--log "Removing frame `%s' as workspace" frame)
  (unless quit
    (let* ((next-frame (exwm-workspace--get-next-workspace frame))
           (following-frames (cdr (memq frame exwm-workspace--list))))
      ;; Need to remove the workspace from the list for the correct calculation of
      ;; indexes below.
      (setq exwm-workspace--list (delete frame exwm-workspace--list))
      ;; Move the windows to the next workspace and switch to it.
      (unless next-frame
        ;; The user managed to delete the last workspace, so create a new one.
        (exwm--log "Last workspace deleted; create a new one")
        (let ((exwm-workspace--create-silently t))
          (setq next-frame (make-frame))))
      (dolist (pair exwm--id-buffer-alist)
        (let ((other-frame (buffer-local-value 'exwm--frame (cdr pair))))
          ;; Move X windows to next-frame.
          (when (eq other-frame frame)
            (exwm-workspace-move-window next-frame (car pair)))
          ;; Update the _NET_WM_DESKTOP property of each following X window.
          (when (memq other-frame following-frames)
            (exwm-workspace--set-desktop (car pair)))))
      ;; If the current workspace is deleted, switch to next one.
      (when (eq frame exwm-workspace--current)
        (exwm-workspace-switch next-frame))))
  ;; Reparent out the frame.
  (let ((outer-id (frame-parameter frame 'exwm-outer-id)))
    (xcb:+request exwm--connection
        (make-instance 'xcb:UnmapWindow
                       :window outer-id))
    (xcb:+request exwm--connection
        (make-instance 'xcb:ReparentWindow
                       :window outer-id
                       :parent exwm--root
                       :x 0
                       :y 0))
    ;; Reset the override-redirect.
    (xcb:+request exwm--connection
        (make-instance 'xcb:ChangeWindowAttributes
                       :window outer-id
                       :value-mask xcb:CW:OverrideRedirect
                       :override-redirect 0))
    ;; Remove fullscreen state.
    (xcb:+request exwm--connection
        (make-instance 'xcb:ewmh:set-_NET_WM_STATE
                       :window outer-id
                       :data nil))
    (xcb:+request exwm--connection
        (make-instance 'xcb:MapWindow
                       :window outer-id)))
  ;; Destroy the container.
  (xcb:+request exwm--connection
      (make-instance 'xcb:DestroyWindow
                     :window (frame-parameter frame 'exwm-container)))
  (xcb:flush exwm--connection)
  ;; Update EWMH properties.
  (exwm-workspace--update-ewmh-props)
  ;; Update switch history.
  (unless quit
    (setq exwm-workspace--switch-history-outdated t)
    (run-hooks 'exwm-workspace-list-change-hook)))

(defun exwm-workspace--on-delete-frame (frame)
  "Hook run upon `delete-frame' removing FRAME as a workspace."
  (cond
   ((not (exwm-workspace--workspace-p frame))
    (exwm--log "Frame `%s' is not a workspace" frame))
   (t
    (exwm-workspace--remove-frame-as-workspace frame))))

(defun exwm-workspace--fullscreen-workspace (frame)
  "Make workspace FRAME fullscreen.
Called from a timer."
  (when (frame-live-p frame)
    (set-frame-parameter frame 'fullscreen 'fullboth)))

(defun exwm-workspace--on-after-make-frame (frame)
  "Hook run upon `make-frame' that configures FRAME as a workspace."
  (cond
   ((exwm-workspace--workspace-p frame)
    (exwm--log "Frame `%s' is already a workspace" frame))
   ((not (display-graphic-p frame))
    (exwm--log "Frame `%s' is not graphical" frame))
   ((not (eq (frame-terminal) exwm--terminal))
    (exwm--log "Frame `%s' is on a different terminal (%S instead of %S)"
               frame
               (frame-terminal frame)
               exwm--terminal))
   ((not (string-equal
          (replace-regexp-in-string "\\.0$" ""
                                    (slot-value exwm--connection 'display))
          (replace-regexp-in-string "\\.0$" ""
                                    (frame-parameter frame 'display))))
    (exwm--log "Frame `%s' is on a different DISPLAY (%S instead of %S)"
               frame
               (frame-parameter frame 'display)
               (slot-value exwm--connection 'display)))
   ((frame-parameter frame 'unsplittable)
    ;; We create floating frames with the "unsplittable" parameter set.
    ;; Though it may not be a floating frame, we won't treat an
    ;; unsplittable frame as a workspace anyway.
    (exwm--log "Frame `%s' is floating" frame))
   (t
    (exwm--log "Adding frame `%s' as workspace" frame)
    (exwm-workspace--add-frame-as-workspace frame))))

(defun exwm-workspace--update-ewmh-props ()
  "Update EWMH properties to match the workspace list."
  (exwm--log)
  (let ((num-workspaces (exwm-workspace--count)))
    ;; Avoid setting 0 desktops.
    (when (= 0 num-workspaces)
      (setq num-workspaces 1))
    ;; Set _NET_NUMBER_OF_DESKTOPS.
    (xcb:+request exwm--connection
        (make-instance 'xcb:ewmh:set-_NET_NUMBER_OF_DESKTOPS
                       :window exwm--root :data num-workspaces))
    ;; Set _NET_DESKTOP_GEOMETRY.
    (exwm-workspace--set-desktop-geometry)
    ;; Update workareas.
    (exwm-workspace--update-workareas))
  (xcb:flush exwm--connection))

(defun exwm-workspace--modify-all-x-frames-parameters (new-x-parameters)
  "Modifies `window-system-default-frame-alist' for the X Window System.
NEW-X-PARAMETERS is an alist of frame parameters, merged into current
`window-system-default-frame-alist' for the X Window System.  The parameters are
applied to all subsequently created X frames."
  (exwm--log)
  ;; The parameters are modified in place; take current
  ;; ones or insert a new X-specific list.
  (let ((x-parameters (or (assq 'x window-system-default-frame-alist)
                          (let ((new-x-parameters '(x)))
                            (push new-x-parameters
                                  window-system-default-frame-alist)
                            new-x-parameters))))
    (setf (cdr x-parameters)
          (append new-x-parameters (cdr x-parameters)))))

(defun exwm-workspace--handle-focus-in (_orig-func _event)
  "Replacement for `handle-focus-in'."
  (interactive "e"))
(put #'exwm-workspace--handle-focus-in 'completion-predicate #'ignore) ;; Move to declare in Emacs 28

(defun exwm-workspace--handle-focus-out (_orig-func _event)
  "Replacement for `handle-focus-out'."
  (interactive "e"))
(put #'exwm-workspace--handle-focus-out 'completion-predicate #'ignore) ;; Move to declare in Emacs 28

(defun exwm-workspace--init-minibuffer-frame ()
  "Initialize minibuffer-only frame."
  (exwm--log)
  ;; Initialize workspaces without minibuffers.
  (setq exwm-workspace--minibuffer
        (make-frame '((window-system . x) (minibuffer . only)
                      (left . 10000) (right . 10000)
                      (width . 1) (height . 1))))
  ;; This is the only usable minibuffer frame.
  (setq default-minibuffer-frame exwm-workspace--minibuffer)
  (exwm-workspace--modify-all-x-frames-parameters
   '((minibuffer . nil)))
  (let ((outer-id (string-to-number
                   (frame-parameter exwm-workspace--minibuffer
                                    'outer-window-id)))
        (window-id (string-to-number
                    (frame-parameter exwm-workspace--minibuffer
                                     'window-id)))
        (container (xcb:generate-id exwm--connection)))
    (set-frame-parameter exwm-workspace--minibuffer
                         'exwm-outer-id outer-id)
    (set-frame-parameter exwm-workspace--minibuffer 'exwm-id window-id)
    (set-frame-parameter exwm-workspace--minibuffer 'exwm-container
                         container)
    (xcb:+request exwm--connection
        (make-instance 'xcb:CreateWindow
                       :depth 0
                       :wid container
                       :parent exwm--root
                       :x 0
                       :y 0
                       :width 1
                       :height 1
                       :border-width 0
                       :class xcb:WindowClass:InputOutput
                       :visual 0
                       :value-mask (logior xcb:CW:BackPixmap
                                           xcb:CW:OverrideRedirect)
                       :background-pixmap xcb:BackPixmap:ParentRelative
                       :override-redirect 1))
    (xcb:+request exwm--connection
        (make-instance 'xcb:ewmh:set-_NET_WM_NAME
                       :window container
                       :data "EXWM minibuffer container"))
    ;; Reparent the minibuffer frame to the container.
    (xcb:+request exwm--connection
        (make-instance 'xcb:ReparentWindow
                       :window outer-id :parent container :x 0 :y 0))
    ;; Map the container.
    (xcb:+request exwm--connection
        (make-instance 'xcb:MapWindow
                       :window container))
    ;; Attach event listener for monitoring the frame
    (xcb:+request exwm--connection
        (make-instance 'xcb:ChangeWindowAttributes
                       :window outer-id
                       :value-mask xcb:CW:EventMask
                       :event-mask xcb:EventMask:StructureNotify))
    (xcb:+event exwm--connection 'xcb:ConfigureNotify
                #'exwm-workspace--on-ConfigureNotify))
  ;; Show/hide minibuffer / echo area when they're active/inactive.
  (add-hook 'minibuffer-setup-hook #'exwm-workspace--on-minibuffer-setup)
  (add-hook 'minibuffer-exit-hook #'exwm-workspace--on-minibuffer-exit)
  (setq exwm-workspace--timer
        (run-with-idle-timer 0 t #'exwm-workspace--on-echo-area-dirty))
  (add-hook 'echo-area-clear-hook #'exwm-workspace--on-echo-area-clear)
  ;; The default behavior of `display-buffer' (indirectly called by
  ;; `minibuffer-completion-help') is not correct here.
  (cl-pushnew '(exwm-workspace--display-buffer) display-buffer-alist
              :test #'equal))

(defun exwm-workspace--exit-minibuffer-frame ()
  "Cleanup minibuffer-only frame."
  (exwm--log)
  ;; Only on minibuffer-frame.
  (remove-hook 'minibuffer-setup-hook #'exwm-workspace--on-minibuffer-setup)
  (remove-hook 'minibuffer-exit-hook #'exwm-workspace--on-minibuffer-exit)
  (remove-hook 'echo-area-clear-hook #'exwm-workspace--on-echo-area-clear)
  (when exwm-workspace--display-echo-area-timer
    (cancel-timer exwm-workspace--display-echo-area-timer))
  (when exwm-workspace--timer
    (cancel-timer exwm-workspace--timer)
    (setq exwm-workspace--timer nil))
  (setq display-buffer-alist
        (cl-delete '(exwm-workspace--display-buffer) display-buffer-alist
                   :test #'equal))
  (setq default-minibuffer-frame nil)
  (when (frame-live-p exwm-workspace--minibuffer) ; might be already dead
    (let ((id (frame-parameter exwm-workspace--minibuffer 'exwm-outer-id)))
      (when (and exwm-workspace--minibuffer id
                 ;; Invoked from `exwm-manage--exit' upon disconnection.
                 (slot-value exwm--connection 'connected))
        (xcb:+request exwm--connection
            (make-instance 'xcb:ReparentWindow
                           :window id
                           :parent exwm--root
                           :x 0
                           :y 0)))
      (setq exwm-workspace--minibuffer nil))))

(defun exwm-workspace--init ()
  "Initialize workspace module."
  (exwm--log)
  ;; Re-initialize the workspace switch-by-name map just in case
  ;; the user customized it via setq instead of setopt. This preserves
  ;; existing behavior and doesn't really add any complexity.
  (exwm-workspace--update-switch-by-name-map)
  ;; Prevent unexpected exit
  (setq exwm-workspace--fullscreen-frame-count 0)
  (exwm-workspace--modify-all-x-frames-parameters
   '((internal-border-width . 0)))
  (let ((initial-workspaces (frame-list)))
    (if (not (exwm-workspace--minibuffer-own-frame-p))
        ;; Initialize workspaces with minibuffers.
        (when (< 1 (length initial-workspaces))
          ;; Exclude the initial frame.
          (dolist (i initial-workspaces)
            (unless (frame-parameter i 'window-id)
              (setq initial-workspaces (delq i initial-workspaces))))
          (let ((f (car initial-workspaces)))
            ;; Remove the possible internal border.
            (set-frame-parameter f 'internal-border-width 0)))
      (exwm-workspace--init-minibuffer-frame)
      ;; Remove/hide existing frames.
      (dolist (f initial-workspaces)
        (when (eq 'x (framep f))        ;do not delete the initial frame.
          (delete-frame f)))
      ;; Recreate one frame with the external minibuffer set.
      (setq initial-workspaces (list (make-frame '((window-system . x))))))
    ;; Prevent `other-buffer' from selecting already displayed EXWM buffers.
    (modify-all-frames-parameters
     '((buffer-predicate . exwm-layout--other-buffer-predicate)))
    ;; Create remaining workspaces.
    (dotimes (_ (- exwm-workspace-number (length initial-workspaces)))
      (nconc initial-workspaces (list (make-frame '((window-system . x))))))
    ;; Configure workspaces
    (let ((exwm-workspace--create-silently t))
      (dolist (i initial-workspaces)
        (exwm-workspace--add-frame-as-workspace i))))
  (xcb:flush exwm--connection)
  ;; We have to advice `x-create-frame' or every call to it would hang EXWM
  (advice-add 'x-create-frame :around #'exwm-workspace--x-create-frame)
  ;; We have to manually handle focus-in and focus-out events for Emacs
  ;; frames.
  (advice-add 'handle-focus-in :around #'exwm-workspace--handle-focus-in)
  (advice-add 'handle-focus-out :around #'exwm-workspace--handle-focus-out)
  ;; Make new frames create new workspaces.
  (add-hook 'after-make-frame-functions
            #'exwm-workspace--on-after-make-frame)
  (add-hook 'delete-frame-functions #'exwm-workspace--on-delete-frame)
  (when (exwm-workspace--minibuffer-own-frame-p)
    (add-hook 'exwm-input--event-hook
              #'exwm-workspace--on-echo-area-clear))
  ;; Switch to the first workspace
  (exwm-workspace-switch 0 t)
  ;; Prevent frame parameters introduced by this module from being
  ;; saved/restored.
  (dolist (i '(exwm-active exwm-outer-id exwm-id exwm-container exwm-geometry
                           exwm-selected-window exwm-urgency fullscreen))
    (unless (assq i frameset-filter-alist)
      (push (cons i :never) frameset-filter-alist))))

(defun exwm-workspace--exit ()
  "Exit the workspace module."
  (exwm--log)
  (when (exwm-workspace--minibuffer-own-frame-p)
    (exwm-workspace--exit-minibuffer-frame))
  (advice-remove 'x-create-frame #'exwm-workspace--x-create-frame)
  (advice-remove 'handle-focus-in #'exwm-workspace--handle-focus-in)
  (advice-remove 'handle-focus-out #'exwm-workspace--handle-focus-out)
  (remove-hook 'after-make-frame-functions
               #'exwm-workspace--on-after-make-frame)
  (remove-hook 'delete-frame-functions
               #'exwm-workspace--on-delete-frame)
  (when (exwm-workspace--minibuffer-own-frame-p)
    (remove-hook 'exwm-input--event-hook
                 #'exwm-workspace--on-echo-area-clear))
  ;; Hide & reparent out all frames (save-set can't be used here since
  ;; X windows will be re-mapped).
  (when (slot-value exwm--connection 'connected)
    (dolist (i exwm-workspace--list)
      (when (frame-live-p i)                    ; might be already dead
        (exwm-workspace--remove-frame-as-workspace i 'quit)
        (modify-frame-parameters i '((exwm-selected-window . nil)
                                     (exwm-urgency . nil)
                                     (exwm-outer-id . nil)
                                     (exwm-id . nil)
                                     (exwm-container . nil)
                                     ;; (internal-border-width . nil) ; integerp
                                     (fullscreen . nil)
                                     (buffer-predicate . nil))))))
  ;; Don't let dead frames linger.
  (setq exwm-workspace--current nil)
  (setq exwm-workspace-current-index 0)
  (setq exwm-workspace--list nil))

(defun exwm-workspace--post-init ()
  "The second stage in the initialization of the workspace module."
  (exwm--log)
  ;; Wait until all workspace frames are resized.
  (with-timeout (1)
    (while (< exwm-workspace--fullscreen-frame-count (exwm-workspace--count))
      (accept-process-output nil 0.1)))
  (setq exwm-workspace--fullscreen-frame-count nil))

(provide 'exwm-workspace)
;;; exwm-workspace.el ends here

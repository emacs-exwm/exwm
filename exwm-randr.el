;;; exwm-randr.el --- RandR Module for EXWM  -*- lexical-binding: t -*-

;; Copyright (C) 2015-2026 Free Software Foundation, Inc.

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

;; This module adds RandR support for EXWM.  Currently it requires external
;; tools such as xrandr(1) to properly configure RandR first.  This
;; dependency may be removed in the future, but more work is needed before
;; that.

;; To use this module, load, enable it and configure
;; `exwm-randr-workspace-monitor-plist' and `exwm-randr-screen-change-hook'
;; as follows:
;;
;;   (setq exwm-randr-workspace-monitor-plist '(0 "VGA1"))
;;   (add-hook 'exwm-randr-screen-change-hook
;;             (lambda ()
;;               (start-process-shell-command
;;                "xrandr" nil "xrandr --output VGA1 --left-of LVDS1 --auto")))
;;   (exwm-randr-mode 1)
;;
;; With above lines, workspace 0 should be assigned to the output named "VGA1",
;; staying at the left of other workspaces on the output "LVDS1".  Please refer
;; to xrandr(1) for the configuration of RandR.

;; References:
;; + RandR (https://www.x.org/archive/X11R7.7/doc/randrproto/randrproto.txt)

;;; Code:

(require 'xcb-randr)

(require 'exwm-core)
(require 'exwm-workspace)

(declare-function x-get-atom-name "C source code" (VALUE &optional FRAME))

(defgroup exwm-randr nil
  "RandR."
  :group 'exwm)

(defvar exwm-randr--connection nil "The X connection.")

(defcustom exwm-randr-refresh-hook nil
  "Normal hook run when the RandR module just refreshed."
  :type 'hook)

(defcustom exwm-randr-screen-change-hook nil
  "Normal hook run when screen changes."
  :type 'hook)

(defcustom exwm-randr-workspace-monitor-plist nil
  "Plist mapping workspaces to monitors.

In RandR 1.5 a monitor is a rectangle region decoupled from the physical
size of screens, and can be identified with `xrandr --listmonitors' (name of
the primary monitor is prefixed with an `*').  When no monitor is created it
automatically fallback to RandR 1.2 output which represents the physical
screen size.  RandR 1.5 monitors can be created with `xrandr --setmonitor'.
For example, to split an output (`LVDS-1') of size 1280x800 into two
side-by-side monitors one could invoke (the digits after `/' are size in mm)

    xrandr --setmonitor *LVDS-1-L 640/135x800/163+0+0 LVDS-1
    xrandr --setmonitor LVDS-1-R 640/135x800/163+640+0 none

If a monitor is not active, the workspaces mapped to it are displayed on the
primary monitor until it becomes active (if ever).  Unspecified workspaces
are all mapped to the primary monitor.  For example, with the following
setting workspace other than 1 and 3 would always be displayed on the
primary monitor where workspace 1 and 3 would be displayed on their
corresponding monitors whenever the monitors are active.

Changes to this variable only take immediate affect when set before
`exwm-randr-mode' is enabled, via `setopt', or when customized (see the
Info node `Customization'). Otherwise, the `exwm-randr-refresh' must be
called explicitly to assign the correct workspaces to the correct monitors.

  \\='(1 \"HDMI-1\" 3 \"DP-1\")"
  :type '(plist :key-type integer :value-type string)
  :initialize 'custom-initialize-changed
  :set (lambda (symbol value)
         (set-default-toplevel-value symbol value)
         (when exwm-randr--connection
           (exwm-randr-refresh))))

(defvar exwm-randr--connection nil "The X connection.")

(defvar exwm-randr--last-timestamp 0 "Used for debouncing events.")

(defvar exwm-randr--prev-screen-change-timestamp 0
  "The most recent ScreenChangeNotify config change timestamp.")

;;;###autoload
(define-minor-mode exwm-randr-mode
  "Toggle EXWM randr support."
  :global t
  :group 'exwm-randr
  (exwm--global-minor-mode-body randr))

(defsubst exwm-randr--assert-connected ()
  "Assert that `exwm-randr-mode' is enabled and activated."
  (cond
   ((not exwm-randr-mode) (user-error "EXWM RandR mode not enabled"))
   ((not exwm-randr--connection) (user-error "EXWM RandR not connected, is EXWM running?"))))

(defun exwm-randr--get-monitors ()
  "Get RandR 1.5 monitors."
  (exwm--log)
  (let (monitor-name geometry monitor-geometry-alist primary-monitor)
    (with-slots (timestamp monitors)
        (xcb:+request-unchecked+reply exwm-randr--connection
            (make-instance 'xcb:randr:GetMonitors
                           :window exwm--root
                           :get-active 1))
      (when (> timestamp exwm-randr--last-timestamp)
        (setq exwm-randr--last-timestamp timestamp))
      (dolist (monitor monitors)
        (with-slots (name primary x y width height) monitor
          (setq monitor-name (x-get-atom-name name)
                geometry (make-instance 'xcb:RECTANGLE
                                        :x x
                                        :y y
                                        :width width
                                        :height height)
                monitor-geometry-alist (cons (cons monitor-name geometry)
                                             monitor-geometry-alist))
          (exwm--log "%s: %sx%s+%s+%s" monitor-name x y width height)
          ;; Save primary monitor when available (fallback to the first one).
          (when (or (/= 0 primary)
                    (not primary-monitor))
            (setq primary-monitor monitor-name)))))
    (exwm--log "Primary monitor: %s" primary-monitor)
    (list primary-monitor monitor-geometry-alist
          (exwm-randr--get-monitor-alias primary-monitor
                                         monitor-geometry-alist))))

(defun exwm-randr--get-monitor-alias (primary-monitor monitor-geometry-alist)
  "Generate monitor aliases using PRIMARY-MONITOR MONITOR-GEOMETRY-ALIST.

In a mirroring setup some monitors overlap and should be treated as one."
  (let (monitor-position-alist monitor-alias-alist monitor-name geometry)
    (setq monitor-position-alist (with-slots (x y)
                                     (cdr (assoc primary-monitor
                                                 monitor-geometry-alist))
                                   (list (cons primary-monitor (vector x y)))))
    (setq monitor-alias-alist (list (cons primary-monitor primary-monitor)))
    (dolist (pair monitor-geometry-alist)
      (setq monitor-name (car pair)
            geometry (cdr pair))
      (unless (assoc monitor-name monitor-alias-alist)
        (let* ((position (vector (slot-value geometry 'x)
                                 (slot-value geometry 'y)))
               (alias (car (rassoc position monitor-position-alist))))
          (if alias
              (setq monitor-alias-alist (cons (cons monitor-name alias)
                                              monitor-alias-alist))
            (setq monitor-position-alist (cons (cons monitor-name position)
                                               monitor-position-alist)
                  monitor-alias-alist (cons (cons monitor-name monitor-name)
                                            monitor-alias-alist))))))
    monitor-alias-alist))

(defun exwm-randr-refresh ()
  "Refresh workspaces according to the updated RandR info."
  (interactive)
  (exwm--log)
  (exwm-randr--assert-connected)
  (let* ((result (exwm-randr--get-monitors))
         (primary-monitor (elt result 0))
         (monitor-geometry-alist (elt result 1))
         (monitor-alias-alist (elt result 2))
         container-monitor-alist container-frame-alist)
    (when (and primary-monitor monitor-geometry-alist)
      (when exwm-workspace--fullscreen-frame-count
        ;; Not all workspaces are fullscreen; reset this counter.
        (setq exwm-workspace--fullscreen-frame-count 0))
      (dotimes (i (exwm-workspace--count))
        (let* ((monitor (plist-get exwm-randr-workspace-monitor-plist i))
               (geometry (cdr (assoc monitor monitor-geometry-alist)))
               (frame (elt exwm-workspace--list i))
               (container (frame-parameter frame 'exwm-container)))
          (if geometry
              ;; Unify monitor names in case it's a mirroring setup.
              (setq monitor (cdr (assoc monitor monitor-alias-alist)))
            ;; Missing monitors fallback to the primary one.
            (setq monitor primary-monitor
                  geometry (cdr (assoc primary-monitor
                                       monitor-geometry-alist))))
          (setq container-monitor-alist (nconc
                                         `((,container . ,(intern monitor)))
                                         container-monitor-alist)
                container-frame-alist (nconc `((,container . ,frame))
                                             container-frame-alist))
          (set-frame-parameter frame 'exwm-randr-monitor monitor)
          (set-frame-parameter frame 'exwm-geometry geometry)))
      ;; Update workareas.
      (exwm-workspace--update-workareas)
      ;; Resize workspace.
      (dolist (f exwm-workspace--list)
        (exwm-workspace--set-fullscreen f))
      (xcb:flush exwm-randr--connection)
      ;; Raise the minibuffer if it's active.
      (when (and (active-minibuffer-window)
                 (exwm-workspace--minibuffer-own-frame-p))
        (exwm-workspace--show-minibuffer))
      ;; Set _NET_DESKTOP_GEOMETRY.
      (exwm-workspace--set-desktop-geometry)
      ;; Update active/inactive workspaces.
      (dolist (w exwm-workspace--list)
        (exwm-workspace--set-active w nil))
      ;; Mark the workspace on the top of each monitor as active.
      (dolist (xwin
               (reverse
                (slot-value (xcb:+request-unchecked+reply exwm-randr--connection
                                (make-instance 'xcb:QueryTree
                                               :window exwm--root))
                            'children)))
        (let ((monitor (cdr (assq xwin container-monitor-alist))))
          (when monitor
            (setq container-monitor-alist
                  (rassq-delete-all monitor container-monitor-alist))
            (exwm-workspace--set-active (cdr (assq xwin container-frame-alist))
                                        t))))
      (xcb:flush exwm-randr--connection)
      (run-hooks 'exwm-randr-refresh-hook))))

(defun exwm-randr--on-ScreenChangeNotify (data _synthetic)
  "Handle `ScreenChangeNotify' event with DATA.

Run `exwm-randr-screen-change-hook' (usually user scripts to configure RandR)."
  (exwm--log)
  (let* ((evt (xcb:unmarshal-new 'xcb:randr:ScreenChangeNotify data))
         (ts (slot-value evt 'config-timestamp)))
    (unless (equal ts exwm-randr--prev-screen-change-timestamp)
      (setq exwm-randr--prev-screen-change-timestamp ts)
      (run-hooks 'exwm-randr-screen-change-hook))))

(defun exwm-randr--on-Notify (data _synthetic)
  "Handle `CrtcChangeNotify' and `OutputChangeNotify' events with DATA.

Refresh when any CRTC/output changes."
  (exwm--log)
  (with-slots (subCode u)
      (xcb:unmarshal-new 'xcb:randr:Notify data)
    (when-let* ((notify
                 (with-slots (cc oc) u
                   (cond
                    ((= subCode xcb:randr:Notify:CrtcChange) cc)
                    ((= subCode xcb:randr:Notify:OutputChange) oc)))))
      (with-slots (timestamp) notify
        (when (> timestamp exwm-randr--last-timestamp)
          (exwm-randr-refresh)
          (setq exwm-randr--last-timestamp timestamp))))))

(defun exwm-randr--on-ConfigureNotify (data _synthetic)
  "Handle `ConfigureNotify' event with DATA.

Refresh when any RandR 1.5 monitor changes."
  (exwm--log)
  (with-slots (window) (xcb:unmarshal-new 'xcb:ConfigureNotify data)
    (when (eq window exwm--root)
      (exwm-randr-refresh))))

(cl-defun exwm-randr--init ()
  "Initialize RandR extension and EXWM RandR module."
  (exwm--log)
  (when exwm-randr--connection
    (cl-return-from exwm-randr--init))
  (setq exwm-randr--connection (xcb:connect))
  (set-process-query-on-exit-flag (slot-value exwm-randr--connection 'process) nil)
  (when (= 0 (slot-value (xcb:get-extension-data exwm-randr--connection 'xcb:randr)
                         'present))
    (xcb:disconnect exwm-randr--connection)
    (setq exwm-randr--connection nil)
    (error "[EXWM] RandR extension is not supported by the server"))
  (with-slots (major-version minor-version)
      (xcb:+request-unchecked+reply exwm-randr--connection
          (make-instance 'xcb:randr:QueryVersion
                         :major-version 1 :minor-version 5))
    (unless (and (= major-version 1) (>= minor-version 5))
      (xcb:disconnect exwm-randr--connection)
      (setq exwm-randr--connection nil)
      (error "[EXWM] The server only support RandR version up to %d.%d"
             major-version minor-version))
    ;; External monitor(s) may already be connected.
    (run-hooks 'exwm-randr-screen-change-hook)
    (exwm-randr-refresh)
    ;; Listen for `ScreenChangeNotify' to notify external tools to
    ;; configure RandR and `CrtcChangeNotify/OutputChangeNotify' to
    ;; refresh the workspace layout.
    (xcb:+event exwm-randr--connection 'xcb:randr:ScreenChangeNotify
                #'exwm-randr--on-ScreenChangeNotify)
    (xcb:+event exwm-randr--connection 'xcb:randr:Notify
                #'exwm-randr--on-Notify)
    (xcb:+event exwm-randr--connection 'xcb:ConfigureNotify
                #'exwm-randr--on-ConfigureNotify)
    (xcb:+request exwm-randr--connection
        (make-instance 'xcb:randr:SelectInput
                       :window exwm--root
                       :enable (logior
                                xcb:randr:NotifyMask:ScreenChange
                                xcb:randr:NotifyMask:CrtcChange
                                xcb:randr:NotifyMask:OutputChange)))
    (xcb:flush exwm-randr--connection)
    (add-hook 'exwm-workspace-list-change-hook #'exwm-randr-refresh))
  ;; Prevent frame parameters introduced by this module from being
  ;; saved/restored.
  (dolist (i '(exwm-randr-monitor))
    (unless (assq i frameset-filter-alist)
      (push (cons i :never) frameset-filter-alist))))

(defun exwm-randr--exit ()
  "Exit the RandR module."
  (exwm--log)
  (remove-hook 'exwm-workspace-list-change-hook #'exwm-randr-refresh)
  (when exwm-randr--connection
    (xcb:disconnect exwm-randr--connection)
    (setq exwm-randr--connection nil)))

(provide 'exwm-randr)
;;; exwm-randr.el ends here

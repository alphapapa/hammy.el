;;; hammy.el --- Programmable, periodic timers for working and taking breaks  -*- lexical-binding: t; -*-

;; Copyright (C) 2022  Adam Porter

;; Author: Adam Porter <adam@alphapapa.net>
;; URL: https://github.com/alphapapa/hammy.el
;; Version: 0.1-pre
;; Package-Requires: ((emacs "28.1") (ts "0.2.2"))
;; Keywords: convenience

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

;; TODO: Pausing.

;;;; FAQ

;; Q: Why are timers called hammys?  Isn't that silly?

;; A: Probably.  But it also helps to distinguish them from Emacs's
;; timers, which are used in the implementation.

;;; Code:

;;;; Requirements

(require 'cl-lib)
(require 'ring)

(require 'ts)

;;;; Structs

(cl-defstruct hammy
  (name "" :type 'string) (documentation "" :documentation "Documentation.")
  (elapsed nil :documentation "List of elapsed intervals."
           ;; FIXME: This.
           )
  (cycles 0 :documentation "Number of times the timer has gone through a cycle of all intervals.")
  (intervals nil :documentation "List of defined intervals.")
  (interval nil :documentation "Current interval, if any.")
  (current-duration)
  (last-duration nil :documentation "Length in seconds of last interval.")
  (timer nil :documentation "Emacs timer for this timer.")
  (etc nil :documentation "Alist which may be used to store any extra data.")
  (before nil :documentation "Function called before running timer.
Called with one argument, the hammy.")
  (after nil :documentation "Function called after timer has completed.
Called with one argument, the hammy.  Called when the hammy's
completion predicate returns non-nil.")
  (complete-p nil :documentation "Predicate that returns non-nil when hammy is complete.
Called with one argument, the hammy.  Called after each interval
is complete, before starting the next interval."))

(cl-defstruct hammy-interval
  (name "" :type 'string)
  (documentation "" :documentation "Documentation.") ; !
  (face nil :documentation "Optional face in which to show the name of the interval.")
  (length nil :documentation "Number of seconds or function that returns such.
If a function, it is given one argument, the timer it is being
run in.")
  (before nil :documentation "Function called before interval begins.
Called with one argument, the Hammy timer.")
  (after nil :documentation "Function called when interval ends.
Called with one argument, the Hammy timer."))

(define-error 'hammy-complete "Hammy is over!")

;;;; Macros

(defmacro hammy-define (name &rest args)
  "Define a new Hammy named NAME made with ARGS.
Returns the hammy, and adds hammy to `hammy-hammys'.  NAME is a
string.  ARGS are passed to `make-hammy', which see.  Within
ARGS, these functions are available to be called:

  `announce':"
  (declare (indent defun))
  ;; FIXME: Docstring.
  
  ;; In some ways, it might be preferable for this macro to expand to
  ;; the hammy struct, but then it wouldn't be forward-compatible if
  ;; the structure changes, so we just expand to the code that makes
  ;; the struct.

  ;; NOTE: If a user byte-compiles a config file containing a
  ;; `hammy-define' call, and the definition of this macro changes in
  ;; a later version, it will be a problem.  So it would be nice if
  ;; this were a function instead of a macro, but that would mean that
  ;; the user would have to quote the argument to prevent evaluation,
  ;; which would likely be confusing to many users.  So, for now, at
  ;; least, it will be a macro.
  `(cl-labels ((announce (message)
                         (lambda (hammy)
                           (hammy-announce hammy message)))
               (notify (message)
                       (lambda (hammy)
                         (hammy-notify hammy message)))
               (duration (interval)
                         (timer-duration interval) )
               (interval (&rest args)
                         (apply #'make-hammy-interval args))
               (num-intervals (hammy)
                              (ring-length (hammy-intervals hammy)))
               (elapsed (hammy)
                        (hammy-elapsed hammy))
               (cycles (hammy)
                       (hammy-cycles hammy))
               (climb (from to &key descend)
                      ;; FIXME: Make arguments optional.
                      (lambda (hammy)
                        (let* ((apex (/ (duration to)
                                        (duration from)))
                               (duration (if (< (cycles hammy) apex)
                                             ;; Spin up!
                                             (min (* (pcase (cycles hammy)
                                                       (0 1)
                                                       (cycles (1+ cycles)))
                                                     (duration from))
                                                  (duration to))
                                           ;; Spin down...
                                           (pcase-exhaustive descend
                                             (`nil (min (* (pcase (cycles hammy)
                                                             (0 1)
                                                             (height (1+ height)))
                                                           (duration from))
                                                        (duration to)))
                                             (`t (hammy-log hammy (format "Descending... (Cycles:%s  Apex:%s  From:%s  To:%s"
                                                                          (cycles hammy) apex from to))
                                                 (min (* (pcase (- (* 2 apex) (cycles hammy))
                                                           (0 1)
                                                           (height (1- height)))
                                                         (duration from))
                                                      (duration to)))))))
                          duration
                          
                          ))))
     (let* ((hammy (make-hammy :name ,name ,@args))
            (ring (make-ring (length (hammy-intervals hammy)))))
       (dolist (interval (hammy-intervals hammy))
         (ring-insert-at-beginning ring interval))
       (setf (hammy-intervals hammy) ring)
       (setf hammy-hammys (cl-delete ,name hammy-hammys :test #'equal :key #'hammy-name))
       (push hammy hammy-hammys)
       hammy)))

;;;; Variables

(defvar hammy-hammys nil
  "List of defined hammys.
Define a hammy with `hammy-define'.")

(hammy-define "Flywheel"
  :documentation "Get your momentum going!"
  :intervals (list (interval :name "Play"
                             :length (climb "5 minutes" "15 minutes")
                             :before (announce "Play time!")
                             :after (announce "Play time is over!"))
                   (interval :name "Work"
                             :length (climb "5 minutes" "45 minutes"
                                            :descend t)
                             :before (announce "Work time!")
                             :after (announce "Work time is over!")))
  :complete-p (lambda (hammy)
                (and (hammy-interval hammy)
                     (equal "Work" (hammy-interval-name (hammy-interval hammy)))
                     (equal (duration "45 minutes") (hammy-current-duration hammy)))))

;;;; Customization

(defgroup hammy nil
  "Programmable interval timers."
  :group 'convenience)

(defcustom hammy-log-buffer-name "*Hammy Log*"
  "Name of Hammy log buffer."
  :type 'string)

(defcustom hammy-start-hook '((lambda (hammy) (hammy-log hammy "Starting...")))
  "Functions run when a hammy is started.
Called with the hammy, and optionally a message."
  :type 'hook)

(defcustom hammy-complete-hook '((lambda (hammy) (hammy-log hammy "Completed.")))
  "Functions run when a hammy is completed.
That is, when the hammy completes its programmed cycles (not when
manually interrupted).  Called with the hammy, and optionally a
message."
  :type 'hook)

(defcustom hammy-interval-hook '((lambda (hammy &optional message) (hammy-log hammy message)))
  "Functions run when a hammy completes an interval.
Called with the hammy, and optionally a message."
  :type 'hook)

(defcustom hammy-cycle-hook '((lambda (hammy) (hammy-log hammy "Cycled.")))
  "Functions run when a hammy completes a cycle.
Called with the hammy, and optionally a message."
  :type 'hook)

;;;; Commands

(defun hammy-stop (hammy &optional quietly)
  "Stop HAMMY timer.
If QUIETLY, don't say so."
  (interactive
   (list (or (hammy-complete "Stop hammy: " (cl-remove-if-not #'hammy-timer hammy-hammys))
             (user-error "No active hammys"))))
  (pcase-let (((cl-struct hammy (timer internal-timer)) hammy))
    (when internal-timer
      (cancel-timer internal-timer)
      (setf (hammy-timer hammy) nil)
      (unless quietly
        (message "Hammy stopped: %s (%s)"
                 ;; TODO: Logging, totals, etc.
                 (hammy-name hammy)
                 (hammy-documentation hammy))))
    (hammy-reset hammy)
    hammy))

(defun hammy-reset (hammy)
  "Reset HAMMY timer.
If already running, restarts it."
  (interactive (list (hammy-complete "Reset hammy: " hammy-hammys)))
  (let ((runningp (hammy-timer hammy)))
    (when runningp
      (hammy-stop hammy 'quietly))
    (setf (hammy-cycles hammy) 0
          (hammy-elapsed hammy) nil
          (hammy-interval hammy) nil)
    (when runningp
      (hammy-start hammy))
    hammy))

(defun hammy-toggle (hammy)
  "Toggle HAMMY timer.
If paused, resume it.  If running, pause it."
  (interactive (list (hammy-complete "Toggle hammy: " hammy-hammys)))
  (if (hammy-timer hammy)
      (let ((remaining-time (float-time (time-subtract (timer--time hammy) (current-time)))))
        (setf (alist-get 'remaining-time (hammy-etc hammy)) remaining-time)
        (hammy-stop hammy))
    (hammy-start hammy (alist-get 'remaining-time (hammy-etc hammy)))
    (setf (alist-get 'remaining-time (hammy-etc hammy)) nil))
  hammy)

;;;; Functions

(defun hammy-start (hammy &optional duration)
  "Start HAMMY and return it.
If DURATION, set its first interval to last that many seconds."
  (interactive (list (hammy-complete "Start hammy: " (cl-remove-if #'hammy-timer hammy-hammys))))
  (unless (and (= 0 (hammy-cycles hammy))
               ;; TODO: Are both of these tests necessary?
               (null (hammy-elapsed hammy))
               (null (hammy-interval hammy)))
    (user-error "Hammy already started: %s" (hammy-format hammy)))
  (run-hook-with-args 'hammy-start-hook hammy)
  (when (hammy-before hammy)
    (funcall (hammy-elapsed hammy) hammy))
  (hammy-next hammy duration)
  ;; (push hammy hammy-active)
  hammy)

(defmacro hammy-run-place (place &rest args)
  `(cl-typecase ,place
     (function (funcall ,place ,@args))
     (null nil)
     (list (dolist (fn ,place)
             (funcall fn ,@args)))))

(defun hammy-next (hammy &optional duration)
  "FIXME: Docstring."
  (unless (and (= 0 (hammy-cycles hammy))
               ;; TODO: Are both of these tests necessary?
               (null (hammy-elapsed hammy))
               (null (hammy-interval hammy)))
    ;; Hammy already started, interval completed.
    (push (hammy-interval hammy) (hammy-elapsed hammy))
    (run-hook-with-args 'hammy-interval-hook hammy
                        (format "Interval ended: %s" (hammy-interval-name (hammy-interval hammy))))
    (hammy-run-place (hammy-interval-after (hammy-interval hammy)) hammy)
    (when (equal (hammy-interval hammy)
                 (ring-ref (hammy-intervals hammy)
                           (1- (ring-length (hammy-intervals hammy)))))
      ;; Cycle completed.
      (cl-incf (hammy-cycles hammy))
      (run-hook-with-args 'hammy-cycle-hook hammy)))
  (if (and (hammy-complete-p hammy)
           (funcall (hammy-complete-p hammy) hammy))
      ;; Hammy complete.
      (progn
        (hammy-stop hammy 'quietly)
        (run-hook-with-args 'hammy-complete-hook hammy)
        (hammy-run-place (hammy-after hammy) hammy)
        ;; (setf hammy-active (remove hammy hammy-active))
        )
    ;; Hammy not complete: start next interval.
    (pcase-let* (((cl-struct hammy interval) hammy)
                 (next-interval (if interval
                                    (ring-next (hammy-intervals hammy) interval)
                                  (ring-ref (hammy-intervals hammy) 0)))
                 (next-duration (or duration
                                    (cl-etypecase (hammy-interval-length next-interval)
                                      (number (hammy-interval-length next-interval))
                                      (function (condition-case _err
                                                    (funcall (hammy-interval-length next-interval) hammy)
                                                  (hammy-complete
                                                   (run-hook-with-args 'hammy-complete-hook hammy)
                                                   (message "Hammy is over!  (%s)" (hammy-name hammy))
                                                   nil)))))))
      (setf (hammy-interval hammy) next-interval
            (hammy-current-duration hammy) next-duration)
      (when next-duration
        (hammy-run-place (hammy-interval-before next-interval) hammy)
        (run-hook-with-args 'hammy-interval-hook hammy
                            (format "Interval started: %s (%s seconds)"
                                    (hammy-interval-name (hammy-interval hammy))
                                    (hammy-current-duration hammy)))
        (setf (hammy-timer hammy) (run-at-time next-duration nil #'hammy-next hammy)))))
  hammy)

(defun hammy-format (hammy &optional message)
  "Return formatted status for HAMMY, optionally with MESSAGE."
  (let* ((interval (cond ((hammy-interval hammy)
                          (format "%s (%s seconds)"
                                  ;; FIXME: Human-format duration.
                                  (hammy-interval-name (hammy-interval hammy))
                                  (ts-human-format-duration (hammy-current-duration hammy) 'abbr)))
                         ((and (hammy-complete-p hammy)
                               (funcall (hammy-complete-p hammy) hammy))
                          "Completed.")
                         (t
                          "None")))
         (message (if message (format "  Message:%S" message) "")))
    (format "Hammy (%s): Interval:%s  Cycles:%s%s"
            (hammy-name hammy) 
            interval
            (hammy-cycles hammy)
            message)))

(defun hammy-log (hammy &optional message)
  "Log MESSAGE for HAMMY to log buffer."
  (let ((inhibit-read-only t))
    (with-current-buffer (hammy--log-buffer)
      (save-excursion
        (goto-char (point-max))
        (insert (format-time-string "%Y-%m-%d %H:%M:%S  ") (hammy-format hammy message) "\n")))))

(defun hammy--log-buffer ()
  "Return Hammy log buffer."
  (with-current-buffer (get-buffer-create hammy-log-buffer-name)
    (read-only-mode)
    (current-buffer)))

(defun hammy-view-log ()
  "Show Hammy log buffer."
  (interactive)
  (pop-to-buffer (hammy--log-buffer)))

(defun hammy-complete (prompt hammys)
  "Return one of HAMMYS selected with completion."
  (pcase (length hammys)
    (0 nil)
    (_ (let* ((selected-hammy-name
               (completing-read prompt (mapcar #'hammy-name hammys)
                                nil t)))
         (cl-find selected-hammy-name hammys
                  :test #'equal :key #'hammy-name)))))

(defun hammy-announce (hammy message)
  (message "Hammy (%s): %s"
           (hammy-name hammy) message))

;;;; Mode

(define-minor-mode hammy-mode
  "Show active hammy in the mode line."
  :global t
  :group 'hammy
  (let ((lighter '(hammy-mode (:eval (hammy-mode-lighter)))))
    (if hammy-mode
        (progn
          (add-hook 'hammy-interval-hook #'force-mode-line-update)
          ;; Avoid adding the lighter multiple times if the mode is activated again.
          (cl-pushnew (list lighter) mode-line-misc-info :test #'equal))
      (remove-hook 'hammy-interval-hook #'force-mode-line-update)
      (setf mode-line-misc-info
            (delete lighter mode-line-misc-info)))))

(defcustom hammy-mode-always-show-lighter t
  "Show lighter even when no hammys are running."
  :type 'boolean)

(defcustom hammy-mode-lighter-prefix "🐹"
  "Show lighter even when no hammys are running."
  :type 'string)

(defface hammy-mode-lighter-prefix-inactive '((t (:inherit error)))
  "Used when no hammy is active.")
(defface hammy-mode-lighter-prefix-active '((t (:inherit font-lock-type-face)))
  "Used when no hammy is active.")

(defun hammy-mode-lighter ()
  ;; NOTE: This actually only shows the first active hammy, but it
  ;; seems unlikely that users will use more than one
  ;; simultaneously.
  (let ((hammy (car (cl-remove-if-not #'hammy-timer hammy-hammys))))
    (if hammy
        (format "%s:%s(%s:%s) "
                (propertize hammy-mode-lighter-prefix
                            'face 'hammy-mode-lighter-prefix-active)
                (hammy-name hammy)
                (propertize (hammy-interval-name (hammy-interval hammy))
                            'face (hammy-interval-face (hammy-interval hammy)))
                (ts-human-format-duration (hammy-current-duration hammy) 'abbr))
      ;; No active hammys.
      (when hammy-mode-always-show-lighter
        (concat (propertize hammy-mode-lighter-prefix
                            'face 'hammy-mode-lighter-prefix-inactive)
                ":None ")))))

;;;; Notifications

(require 'notifications)

(defun hammy-notify (hammy &optional message)
  (notifications-notify :title (format "Hammy (%s)"
                                       (hammy-name hammy))
                        :body (or message (hammy-format hammy))))

;;;; Footer

(provide 'hammy)

;;; hammy.el ends here

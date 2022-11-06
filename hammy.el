;;; hammy.el --- Programmable, interactive interval timers  -*- lexical-binding: t; -*-

;; Copyright (C) 2022  Adam Porter

;; Author: Adam Porter <adam@alphapapa.net>
;; URL: https://github.com/alphapapa/hammy.el
;; Version: 0.2.1
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

;; This library provides programmable, interactive interval timers for
;; Emacs.  They can be used, e.g. to alternate between working and
;; resting periods, to remind yourself to stretch your legs, etc.
;; Timers, called "hammys," are easily defined by the user to behave
;; as desired, and they can be integrated into other programs by
;; calling Lisp functions in the hammys' definitions.

;;;; FAQ

;; Q: Why are timers called hammys?  Isn't that silly?

;; A: Probably, but is it sillier than calling them tomatoes?
;; Besides, it helps to distinguish them from Emacs's timers, which
;; are used in the implementation.

;;; Code:

;; TODO: Pausing.
;; TODO: Summarize logged interval times.
;; TODO: Menu bar/clicking for mode line lighter.  See `tab-bar-menu-bar', et al.

;;;; Requirements

(require 'cl-lib)
(require 'map)
(require 'ring)

(eval-when-compile
  ;; For `org-with-point-at'.
  (require 'org-macs))

(require 'ts)

;;;; Structs

(cl-defstruct hammy
  (name "" :type 'string) (documentation "" :documentation "Documentation.")
  (history nil :documentation "List of elapsed intervals.
Each element is a list of three elements: the interval, the time
it began, and the time it ended.")
  (cycles 0 :documentation "Number of times the timer has gone through a cycle of all intervals.")
  (intervals nil :documentation "List of defined intervals.")
  (interval nil :documentation "Current interval, if any.")
  (current-interval-start-time nil :documentation "Time at which the current interval started.")
  (current-duration)
  (last-duration nil :documentation "Length in seconds of last interval.")
  (timer nil :documentation "Emacs timer for this timer.")
  (etc nil :documentation "Alist which may be used to store any extra data.")
  (before nil :documentation "Function called before running timer.
Called with one argument, the hammy.")
  (after nil :documentation "Function(s) called after timer has completed.
Called with one argument, the hammy.  Called when the hammy's
completion predicate returns non-nil.")
  (stopped nil :documentation "Function(s) called after stopping timer.
Called with one argument, the hammy.  Called by `hammy-stop'.")
  (complete-p nil :documentation "Predicate that returns non-nil when hammy is complete.
Called with one argument, the hammy.  Called after each interval
is complete, before starting the next interval.")
  (overduep))

(cl-defstruct hammy-interval
  (name "" :type 'string)
  (documentation "" :documentation "Documentation.") ; !
  (face nil :documentation "Optional face in which to show the name of the interval.")
  (duration nil :documentation "Number of seconds or function that returns such.
If a function, it is given one argument, the timer it is being
run in.")
  (before nil :documentation "Function(s) called before interval begins.
Called with one argument, the Hammy timer.")
  (after nil :documentation "Function(s) called when interval ends.
Called with one argument, the Hammy timer.  Note that when an
interval's `advance' slot is not `auto', the `after' slot's
functions are not called until the user manually advances to the
next interval.")
  (advance 'auto :documentation "How to advance to the next interval when this one ends.
If `auto', do so automatically.  Otherwise, a list of functions
to call when the interval is ready to be advanced, and don't
advance until the user calls `hammy-next'."))

(define-error 'hammy-complete "Hammy is over!")

;;;; Macros

;;;###autoload
(defmacro hammy-define (name &rest args)
  "Define a new Hammy named NAME made with ARGS.
Returns the hammy, and adds hammy to `hammy-hammys'.  NAME is a
string.  ARGS are passed to `make-hammy', which see.  Useful ones
include:

  `:documentation': An optional documentation string.

  `:intervals': A list of intervals.  Each one is defined with
    the local function `interval', which calls
    `make-hammy-interval', which see for its arguments.

  `:before': One or a list of functions which are called when the
    interval begins.  See the `do' macro, documented later.

  `:after': One or a list of functions which are called when the
    interval ends.  See the `do' macro, documented later.

  `:duration': A number of seconds, or a string passed to
    `timer-duration' to return such, or a function which returns
    such (called before starting the interval each cycle).  See
    the `do' macro, documented later.

  `:advance': Nil to advance automatically, or one or a list of
    functions to call when the interval's timer has elapsed and
    the user should be prompted to manually advance to the next
    interval.  See the `do' and `remind' macros, documented
    later.

Within ARGS, these pseudo-functions and forms available:

  `announce (message)': Announce MESSAGE in the echo area.
  `notify (message)`: Send MESSAGE as a desktop notification.

  `climb (from to &key descend step)': Return a function that
    returns a duration gradually increasing from FROM to TO, and
    optionally decreasing back to TO, by STEP.  FROM, TO, and
    STEP may be numbers or strings (passed to `timer-duration',
    which see).  DESCEND, if non-nil, causes the duration to
    gradually decrease back to FROM after reaching TO.

  `duration (interval)': Return a number of seconds equivalent to
    INTERVAL (a string like \"10 minutes\").  Calls
    `timer-duration', which see.

  `do (&rest body)': Expands to a lambda that binds `hammy' to
    the current hammy and evaluates BODY.  Within its BODY, these
    forms are bound:

    `current-duration': The duration in seconds of the current interval.
    `current-interval-start-time': The time at which the current interval began.
    `cycles': The number of cycles the hammy has completed.
    `etc': The hammy's `etc' slot.
    `history': The hammy's history list.
    `interval': The current interval (a `hammy-interval' struct).
    `interval-name': The name of the current interval.

  `elapsed (&optional interval)': Calls `hammy-elapsed' with the
    hammy, which see.

  `interval (&rest args)': Calls `make-hammy-interval', which
    see.

  `num-intervals ()': Returns the hammy's number of intervals.

  `remind (delay &rest fns)': Return a function that is called
    every DELAY seconds until the interval is manually advanced,
    calling FNS each time.  (The function automatically makes
    necessary adjustments to the hammy to set and cancel the
    periodic reminders.)

  `run (command)': Runs COMMAND (a string) asynchronously with
    `make-process', discarding its output and return value."
  (declare (indent defun))
  ;; In some ways, it might be preferable for this macro to expand to
  ;; the hammy struct, but then it wouldn't be forward-compatible if
  ;; the structure changes, so we just expand to the code that makes
  ;; the struct.

  ;; NOTE: This macro essentially expands to a call to `make-hammy'
  ;; with the given arguments, wrapping some of them in macrolet,
  ;; symbol-macrolet, and labels forms, and lambdas.  This way, we get
  ;; some checking for free at macro expansion time and compilation
  ;; time, rather than those errors happening at runtime.

  ;; NOTE: If a user byte-compiles a config file containing a
  ;; `hammy-define' call, and the definition of this macro changes in
  ;; a later version, it will be a problem.  So it would be nice if
  ;; this were a function instead of a macro, but that would mean that
  ;; the user would have to quote the argument to prevent evaluation,
  ;; which would likely be confusing to many users.  So, for now, at
  ;; least, it will be a macro.
  `(cl-macrolet ((announce (message)
                           `(hammy-announce hammy ,message))
                 (notify (message)
                         `(hammy-notify hammy ,message))
                 (cycles ()
                         `(hammy-cycles hammy))
                 (listify (place)
                          `(unless (listp ,place)
                             (setf ,place (list ,place))))
                 (listify-functions (place)
                                    `(when (functionp ,place)
                                       (setf ,place (list ,place))))
                 (do (&rest body)
                     `(lambda (hammy)
                        (cl-symbol-macrolet ((current-duration (hammy-current-duration hammy))
                                             (current-interval-start-time (hammy-current-interval-start-time hammy))
                                             (cycles (hammy-cycles hammy))
                                             (etc (hammy-etc hammy))
                                             (history (hammy-history hammy))
                                             (interval (hammy-interval hammy))
                                             (interval-name (hammy-interval-name interval)))
                          (ignore hammy)
                          ,@body))))
     ;; NOTE: Some of these functions are called at "hammy time" (I
     ;; know...), while others return lambdas to be called at hammy
     ;; time.
     (cl-labels ((run (command)
                      ;; This makes it easier to run a shell command without
                      ;; having to hack around `async-shell-command' to prevent
                      ;; it from displaying an output buffer, or deal with
                      ;; `call-process's awkward arguments.
                      (let* ((command (split-string command))
                             (name (format "hammy: Calling process %S" (car command))))
                        (make-process :name name :command command :noquery t)))
                 (duration (interval)
                           (timer-duration interval) )
                 (interval (&rest args)
                           (apply #'make-hammy-interval args))
                 (elapsed (hammy &optional interval)
                          "Call `hammy-elapsed', which see."
                          (hammy-elapsed hammy interval))
                 (num-intervals (hammy)
                                (ring-length (hammy-intervals hammy)))
                 (history (hammy)
                          (hammy-history hammy))
                 (cycles (hammy)
                         (hammy-cycles hammy))
                 (climb (from to &key descend step)
                        (lambda (hammy)
                          (let* ((from (duration from))
                                 (to (duration to))
                                 (apex (/ to from))
                                 (step (cl-typecase step
                                         (string (duration step))
                                         (number step)))
                                 (duration (cl-labels
                                               ((ascend
                                                 () (min (* (pcase (cycles hammy)
                                                              (0 1)
                                                              (height (1+ height)))
                                                            from)
                                                         to))
                                                (descend
                                                 () (min (* (pcase (- (* 2 apex) (cycles hammy))
                                                              (0 1)
                                                              (height (1- height)))
                                                            from)
                                                         to)))
                                             (if (< (cycles hammy) apex)
                                                 ;; Spin up!
                                                 (if step
                                                     (+ from (* step (cycles hammy)))
                                                   (ascend))
                                               ;; Spin down...
                                               (pcase-exhaustive descend
                                                 (`nil (ascend))
                                                 (`t (hammy-log hammy
                                                                (format "Descending... (Cycles:%s  Apex:%s  From:%s  To:%s  Step:%s"
                                                                        (cycles hammy) apex from to step))
                                                     (if step
                                                         (+ from (* step (- (* 2 apex) (cycles hammy))))
                                                       (descend))))))))
                            duration)))
                 (remind (delay &rest fns)
                         (lambda (hammy)
                           (listify (hammy-after hammy))
                           (cl-pushnew #'cancel-reminder (hammy-after hammy))
                           (dolist (fn fns)
                             (funcall fn hammy))
                           (let ((delay-secs (duration delay)))
                             ;; TODO: Allow the duration to also be a function to return the reminder delay.
                             (setf (alist-get 'reminder (hammy-etc hammy))
                                   (run-with-timer delay-secs delay-secs
                                                   (lambda (hammy)
                                                     (dolist (fn fns)
                                                       (funcall fn hammy)))
                                                   hammy)))
                           ;; TODO: Might need to cancel and restart
                           ;; the reminder when a hammy is paused,
                           ;; too.
                           (listify (hammy-interval-after (hammy-interval hammy)))
                           (cl-pushnew #'cancel-reminder (hammy-interval-after (hammy-interval hammy)))))
                 (cancel-reminder (hammy)
                                  (when (alist-get 'reminder (hammy-etc hammy))
                                    (cancel-timer (alist-get 'reminder (hammy-etc hammy)))
                                    (setf (alist-get 'reminder (hammy-etc hammy)) nil))))
       (let* ((hammy (make-hammy :name ,name ,@args))
              (ring (make-ring (length (hammy-intervals hammy)))))
         (dolist (interval (hammy-intervals hammy))
           (progn
             ;; Ensure that function slots are lists of functions, not just a function.
             (listify-functions (hammy-interval-before interval))
             (listify-functions (hammy-interval-after interval))
             (listify-functions (hammy-interval-advance interval)))
           (ring-insert-at-beginning ring interval))
         (setf (hammy-intervals hammy) ring)
         (setf hammy-hammys (cl-delete ,name hammy-hammys :test #'equal :key #'hammy-name))
         (push hammy hammy-hammys)
         hammy))))

(defun hammy-call (fn-or-fns &rest args)
  "Call FN-OR-FNS with ARGS.
If FN-OR-FNS is a function, call it; if a list of functions, call
each of them; if nil, do nothing."
  (cl-typecase fn-or-fns
    (null nil)
    (function (apply fn-or-fns args))
    (list (dolist (fn fn-or-fns)
            (apply fn args)))))

;;;; Variables

(defvar org-clock-hd-marker)

(defvar hammy-hammys nil
  "List of defined hammys.
Define a hammy with `hammy-define'.")

(defvar hammy-active nil
  "List of active hammys.")

;;;; Customization

(defgroup hammy nil
  "Programmable interval timers."
  :group 'convenience)

(defcustom hammy-log-buffer-name "*Hammy Log*"
  "Name of Hammy log buffer."
  :type 'string)

(defcustom hammy-start-hook '((lambda (hammy) (hammy-log hammy "Starting...")))
  "Functions run when a hammy is started.
Called with the hammy."
  :type 'hook)

(defcustom hammy-stopped '((lambda (hammy)
                             (hammy-log hammy "Stopped.")
                             (hammy-log hammy (hammy-summary hammy))))
  "Functions run when a hammy is stopped.
Called with the hammy."
  :type 'hook)

(defcustom hammy-complete-hook '((lambda (hammy)
                                   (hammy-log hammy "Completed.")
                                   (hammy-log hammy (hammy-summary hammy))))
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

(defcustom hammy-sound-end-break nil
  "Play this sound when a break interval ends."
  :type '(choice file (const :tag "No sound" nil)))

(defcustom hammy-sound-end-work nil
  "Play this sound when a work interval ends."
  :type '(choice file (const :tag "No sound" nil)))

;;;; Commands

(defun hammy-adjust (hammy)
  "Adjust HAMMY's interval durations."
  (interactive (list (hammy-complete "Adjust hammy: " hammy-hammys)))
  ;; TODO: Reset durations in `hammy-reset'.
  (cl-labels ((adjust-interval
               (interval) (cl-symbol-macrolet ((original-duration
                                                (alist-get interval (alist-get 'original-durations (hammy-etc hammy)))))
                            (let* ((old-duration (hammy-interval-duration interval))
                                   (input-duration (read-string (format "New duration (number, function, or quoted-string duration) for interval \"%s\": "
                                                                        (hammy-interval-name interval))
                                                                nil nil (prin1-to-string old-duration)))
                                   (new-duration (unless (string-empty-p input-duration)
                                                   ;; TODO: Allow the user to type, e.g. "25 minutes" without enclosing quotes.
                                                   (car (read-from-string input-duration)))))
                              (when new-duration
                                (cl-check-type new-duration (or number function string))
                                (unless original-duration
                                  ;; Only save the original duration the first time the interval is adjusted.
                                  (setf original-duration old-duration))
                                (setf (hammy-interval-duration interval) new-duration))))))
    (mapc #'adjust-interval (ring-elements (hammy-intervals hammy)))))

;;;###autoload
(cl-defun hammy-start (hammy &key duration interval)
  "Start HAMMY and return it.
If DURATION, set its first interval to last that many seconds.
INTERVAL may be an interval in the hammy to start
with (interactively, with universal prefix, prompt for the
interval with completion)."
  (interactive (let ((hammy (hammy-complete "Start hammy: " (cl-remove-if #'hammy-timer hammy-hammys))))
                 (list hammy
                       :duration (cl-typecase current-prefix-arg
                                   (number current-prefix-arg))
                       :interval (cl-typecase current-prefix-arg
                                   (null nil)
                                   (list (hammy-complete-interval hammy :prompt "Start with interval: "))))))
  (when (hammy-interval hammy)
    (user-error "Hammy already started: %s" (hammy-format hammy)))
  (run-hook-with-args 'hammy-start-hook hammy)
  (hammy-call (hammy-before hammy) hammy)
  (hammy-next hammy :duration duration :advance t :interval interval)
  (push hammy hammy-active)
  hammy)

(declare-function org-before-first-heading-p "org")
;;;###autoload
(defun hammy-start-org-clock-in (&rest _ignore)
  "Call `org-clock-in' and start a hammy (or use an already-started one).
If point is in an Org entry, clock into it; otherwise, offer a
list of recently clocked tasks to clock into.  The Org task will
then automatically be clocked out during the hammy's second
interval (and when the hammy is stopped), and back in when the
first interval resumes.  (If the user clocks into a different
task while the hammy is running, the task that is clocked-in when
the work interval ends will be clocked back into when the next
work interval begins.)

Returns the hammy from `hammy-start'.  Assumes that the hammy's
first interval is the work interval (i.e. the one during which
the task should be clocked in)."
  (interactive)
  (require 'org)
  ;; MAYBE: Take a point-or-marker argument for the task to clock into.
  (if (and (eq major-mode 'org-mode)
           (not (org-before-first-heading-p)))
      ;; At an Org entry: clock in to heading at point.
      (org-clock-in)
    ;; Not in an Org entry: offer a list to choose from.
    (org-clock-in '(4)))
  (let ((hammy (hammy-complete "Clock in with Hammy: " hammy-hammys)))
    (unless (hammy-interval hammy)
      (hammy-start hammy))
    (cl-macrolet ((pushfn (fn place)
                          `(cl-pushnew ,fn ,place :test #'equal)))
      (pushfn #'hammy--org-clock-in (hammy-interval-before (hammy-interval hammy)))
      (pushfn #'hammy--org-clock-out (hammy-interval-after (hammy-interval hammy)))
      (pushfn #'hammy--org-clock-out (hammy-stopped hammy)))
    hammy))

(defun hammy-stop (hammy &optional quietly)
  "Stop HAMMY timer.
If QUIETLY, don't say so."
  (interactive
   (list (or (hammy-complete "Stop hammy: " hammy-active)
             (user-error "No active hammys"))))
  (pcase-let* (((cl-struct hammy (timer internal-timer)
                           (etc (map reminder)))
                hammy)
               ;; TODO: Logging, totals, etc.
               (message "Stopped."))
    (when internal-timer
      (cancel-timer internal-timer)
      (setf (hammy-timer hammy) nil)
      (hammy-log hammy message)
      (unless quietly
        (message message)))
    (when reminder
      (cancel-timer reminder)
      (setf (alist-get 'reminder (hammy-etc hammy)) nil))
    ;; Run the hook after having stopped the hammy, so any errors in
    ;; stopped functions won't prevent the hammy from stopping
    ;; correctly; and do it before resetting the hammy, so functions
    ;; in the stop hook can access the hammy's data before resetting.
    (hammy--record-interval hammy)
    (run-hook-with-args 'hammy-stopped hammy)
    (hammy-call (hammy-stopped hammy) hammy)
    (setf (hammy-interval hammy) nil
          hammy-active (remove hammy hammy-active))
    hammy))

(cl-defun hammy-next (hammy &key duration advance interval)
  "Advance to HAMMY's next interval.
If DURATION (interactively, with numeric prefix), set the
interval's duration to DURATION seconds.  If ADVANCE, advance to
the next interval even if the previous interval has an
unsatisfied ADVANCE predicate.  INTERVAL may be an interval in
the hammy to advance to (interactively, with universal prefix,
prompt for the interval with completion)."
  (interactive (if-let ((hammy (hammy-complete "Advance hammy: " hammy-active)))
                   (list hammy
                         :duration (cl-typecase current-prefix-arg
                                     (number current-prefix-arg))
                         :advance t
                         :interval (cl-typecase current-prefix-arg
                                     (null nil)
                                     (list (hammy-complete-interval hammy :prompt "Advance to interval: "))))
                 (user-error (substitute-command-keys "No active hammys (use \"\\[hammy-start]\")"))))
  (when (hammy-timer hammy)
    ;; Cancel any outstanding timer.
    (cancel-timer (hammy-timer hammy))
    (setf (hammy-timer hammy) nil))
  (cl-labels ((advancep
               () (or (and (hammy-interval hammy)
                           (eq 'auto (hammy-interval-advance (hammy-interval hammy))))
                      advance)))
    (when (hammy-interval hammy)
      ;; Hammy already started, interval completed (or ready to be
      ;; advanced).
      (when (and (advancep)
                 (equal (hammy-interval hammy)
                        (ring-ref (hammy-intervals hammy)
                                  (1- (ring-length (hammy-intervals hammy))))))
        ;; Cycle completed.
        (cl-incf (hammy-cycles hammy))
        ;; TODO: Not sure if it makes sense to run the cycle hook here
        ;; or later, after running other hooks.
        (run-hook-with-args 'hammy-cycle-hook hammy)))
    (if (and (advancep)
             (hammy-complete-p hammy)
             (funcall (hammy-complete-p hammy) hammy))
        ;; Hammy complete.
        (progn
          (hammy-stop hammy 'quietly)
          (run-hook-with-args 'hammy-complete-hook hammy)
          (hammy-call (hammy-after hammy) hammy))
      ;; Hammy not complete: start next interval.
      (pcase-let* (((cl-struct hammy (interval current-interval)) hammy)
                   (next-interval (or interval
                                      (if current-interval
                                          (ring-next (hammy-intervals hammy) current-interval)
                                        (ring-ref (hammy-intervals hammy) 0))))
                   (next-duration (or duration
                                      ;; This seems a bit awkward, but we want to allow the value to be a
                                      ;; number, a string, or a function that returns a number or string.
                                      (pcase-exhaustive
                                          (cl-etypecase (hammy-interval-duration next-interval)
                                            (function (condition-case _err
                                                          (funcall (hammy-interval-duration next-interval) hammy)
                                                        (hammy-complete
                                                         (run-hook-with-args 'hammy-complete-hook hammy)
                                                         (message "Hammy is over!  (%s)" (hammy-name hammy))
                                                         nil)))
                                            ((or number string) (hammy-interval-duration next-interval)))
                                        ((and (pred numberp) it) it)
                                        ((and (pred stringp) it) (timer-duration it))))))
        (if (not (advancep))
            ;; Interval requires manual advancing.
            (progn
              (hammy-log hammy "Waiting for user to advance...")
              (setf (hammy-overduep hammy) t)
              (hammy-call (hammy-interval-advance (hammy-interval hammy)) hammy))
          ;; Automatically advancing, manually advancing, or starting the hammy.
          (when (hammy-interval hammy)
            ;; Advancing to the next interval (rather than starting the hammy).
            ;; NOTE: We call the interval-hook and the interval's after
            ;; functions when actually advancing to the next interval.
            (hammy--record-interval hammy)
            (hammy-log hammy (format "Elapsed: %s" (hammy-format-current-times hammy)))
            (run-hook-with-args 'hammy-interval-hook hammy
                                (format "Interval ended: %s"
                                        (hammy-interval-name (hammy-interval hammy))))
            (hammy-call (hammy-interval-after (hammy-interval hammy)) hammy))
          (setf (hammy-interval hammy) next-interval
                (hammy-current-interval-start-time hammy) (current-time)
                (hammy-current-duration hammy) next-duration
                (hammy-overduep hammy) nil)
          (when next-duration
            ;; Starting next interval.
            (hammy-call (hammy-interval-before next-interval) hammy)
            ;; TODO: Mention elapsed time of just-completed interval.
            (run-hook-with-args 'hammy-interval-hook hammy
                                (format "Interval started: %s (%s)"
                                        (hammy-interval-name (hammy-interval hammy))
                                        (ts-human-format-duration (hammy-current-duration hammy) 'abbr)))
            (setf (hammy-timer hammy) (run-at-time next-duration nil #'hammy-next hammy)))))))
  hammy)

(defun hammy-reset (hammy)
  "Reset HAMMY timer.
If already running, restarts it."
  (interactive (list (hammy-complete "Reset hammy: " hammy-hammys)))
  (let ((runningp (hammy-timer hammy)))
    (when runningp
      (hammy-stop hammy 'quietly))
    (setf (hammy-cycles hammy) 0
          (hammy-etc hammy) nil
          (hammy-history hammy) nil
          (hammy-interval hammy) nil
          (hammy-current-interval-start-time hammy) nil
          (hammy-overduep hammy) nil)
    (when (alist-get 'original-durations (hammy-etc hammy))
      ;; Restore any original durations.
      (cl-loop for (interval . duration) in (alist-get 'original-durations (hammy-etc hammy))
               do (progn
                    (setf (hammy-interval-duration interval) duration)
                    (cl-callf2 assoc-delete-all 'original-durations (hammy-etc hammy)))))
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

(cl-defun hammy-complete-interval (hammy &key (prompt "Interval: "))
  "Return an interval selected in HAMMY with completion.
PROMPT may be specified."
  (let* ((intervals (ring-elements (hammy-intervals hammy)))
         (names (mapcar #'hammy-interval-name intervals))
         (selected-name (completing-read prompt names nil t)))
    (cl-find selected-name intervals
             :test (lambda (name interval)
                     (equal name (hammy-interval-name interval))))))

(defun hammy-format (hammy &optional message)
  "Return formatted status for HAMMY, optionally with MESSAGE."
  (let* ((interval (cond ((hammy-interval hammy)
                          (format "%s (%s)"
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
  (with-current-buffer (hammy-log-buffer)
    (let* ((inhibit-read-only t)
           (buffer-window (get-buffer-window (current-buffer)))
           (point-at-eob-p (equal (point-max)
                                  (if buffer-window
                                      (window-point buffer-window)
                                    (point)))))
      (save-excursion
        (goto-char (point-max))
        (insert (format-time-string "%Y-%m-%d %H:%M:%S  ") (hammy-format hammy message) "\n"))
      (when point-at-eob-p
        (if buffer-window
            (setf (window-point buffer-window) (point-max))
          (setf (point) (point-max)))))))

(defun hammy-format-current-times (hammy)
  "Return current times for HAMMY formatted.
String includes elapsed time of the current interval and any
overrun time."
  (let* ((elapsed-secs (float-time
                        (time-subtract (current-time)
                                       (hammy-current-interval-start-time hammy))))
         (difference (float-time
                      (time-subtract (hammy-current-duration hammy) elapsed-secs))))
    (format "%s%s"
            (ts-human-format-duration elapsed-secs 'abbr)
            (if (not (zerop difference))
                (let ((sign (if (< 0 difference) "-" "+")))
                  (format " (%s%s)" sign (ts-human-format-duration (abs difference) 'abbr)))
              ""))))

(defun hammy-elapsed (hammy &optional interval)
  "Return HAMMY's elapsed time in seconds.
If INTERVAL (an interval struct or an interval name string),
return the elapsed time for that interval (summed across all
cycles)."
  (pcase-let* (((cl-struct hammy history) hammy)
               (interval-history
                (cl-typecase interval
                  (hammy-interval (cl-remove-if-not (lambda (element)
                                                      (eq interval (car element)))
                                                    history))
                  (string (cl-remove-if-not (lambda (element)
                                              (equal interval (hammy-interval-name (car element))))
                                            history))
                  (t history))))
    (cl-loop for (_interval start-time end-time) in interval-history
             sum (float-time (time-subtract end-time start-time)))))

(defun hammy-complete (prompt hammys)
  "Return one of HAMMYS selected with completion and PROMPT."
  (cl-labels ((describe (hammy)
                        (format "%s (%s)"
                                (hammy-name hammy)
                                (hammy-documentation hammy))))
    (pcase (length hammys)
      (0 nil)
      (1 (car hammys))
      (_ (let* ((map (cl-loop for hammy in hammys
                              collect (cons (describe hammy) hammy)))
                (description (completing-read prompt map nil t)))
           (alist-get description map nil nil #'equal))))))

(defun hammy-announce (hammy message)
  "Announce MESSAGE in the echo area for HAMMY."
  (message "Hammy (%s): %s"
           (hammy-name hammy) message))

(defun hammy-summary (hammy)
  "Return a summary string for HAMMY.
Summary includes elapsed times, etc."
  (format "Total elapsed:%s  Intervals:%s  Cycles:%s"
          (ts-human-format-duration (hammy-elapsed hammy) 'abbr)
          (mapconcat (lambda (interval)
                       (format "(%s:%s)"
                               (hammy-interval-name interval)
                               (ts-human-format-duration (hammy-elapsed hammy interval) 'abbr)))
                     (ring-elements (hammy-intervals hammy))
                     "")
          (hammy-cycles hammy)))

(declare-function org-clock-in "org-clock")
(defun hammy--org-clock-in (hammy)
  "Clock in to HAMMY's Org task."
  (cl-symbol-macrolet ((marker (alist-get 'org-clock-hd-marker (hammy-etc hammy))))
    (when marker
      (org-with-point-at marker
        (org-clock-in))
      ;; Unset the saved marker, because it will be saved again when
      ;; clocking out.
      (setf marker nil))))

(declare-function org-clocking-p "org-clock")
(declare-function org-clock-out "org-clock")
(defun hammy--org-clock-out (hammy)
  "Clock out of HAMMY's Org task."
  (when (org-clocking-p)
    ;; Record the clocked-in task so we can clock back in to it later.
    ;; `org-clock-out' kills the marker, so we have to copy it for
    ;; future reference.
    (setf (alist-get 'org-clock-hd-marker (hammy-etc hammy))
          (copy-marker org-clock-hd-marker))
    (org-clock-out)))

(defun hammy--record-interval (hammy)
  "Record current interval in HAMMY's history."
  (push (list (hammy-interval hammy)
              (hammy-current-interval-start-time hammy)
              (current-time))
        (hammy-history hammy)))

;;;; Mode

(defcustom hammy-mode-always-show-lighter t
  "Show lighter even when no hammys are running."
  :type 'boolean)

(defcustom hammy-mode-lighter-prefix "🐹"
  "Show lighter even when no hammys are running."
  :type 'string)

(defcustom hammy-mode-lighter-overdue "!"
  "Shown when hammy is due to be manually advanced."
  :type 'string)

(defcustom hammy-mode-lighter-suffix-inactive "None"
  "Shown when no hammys are running."
  :type 'string)

(defcustom hammy-mode-update-mode-line-continuously t
  "Update the mode line every second while a hammy is running."
  :type 'boolean)

(defface hammy-mode-lighter-prefix-inactive '((t (:inherit warning)))
  "Used when no hammy is active.")

(defface hammy-mode-lighter-prefix-active '((t (:inherit font-lock-type-face)))
  "Used when no hammy is active.")

(defface hammy-mode-lighter-overdue '((t (:inherit error)))
  "Used when no hammy is active.")

(defvar hammy-mode-update-mode-line-timer nil
  "Timer used to update the mode line.")

;;;###autoload
(define-minor-mode hammy-mode
  "Show active hammy in the mode line."
  :global t
  (let ((lighter '(hammy-mode (:eval (hammy-mode-lighter)))))
    (if hammy-mode
        (progn
          (when hammy-mode-update-mode-line-continuously
            ;; TODO: Only run this timer when a hammy is running.
            (when (timerp hammy-mode-update-mode-line-timer)
              ;; Cancel any existing timer.  Generally shouldn't happen, but not impossible.
              (cancel-timer hammy-mode-update-mode-line-timer))
            (setf hammy-mode-update-mode-line-timer (run-with-timer 1 1 #'hammy--mode-line-update)))
          (add-hook 'hammy-interval-hook #'hammy--mode-line-update)
          ;; Avoid adding the lighter multiple times if the mode is activated again.
          (cl-pushnew lighter global-mode-string :test #'equal))
      (when hammy-mode-update-mode-line-timer
        (cancel-timer hammy-mode-update-mode-line-timer)
        (setf hammy-mode-update-mode-line-timer nil))
      (remove-hook 'hammy-interval-hook #'hammy--mode-line-update)
      (setf global-mode-string
            (remove lighter global-mode-string)))))

(defun hammy-mode-lighter ()
  "Return the mode-line lighter for `hammy-mode'."
  (cl-labels ((format-hammy
               (hammy) (let ((remaining (abs
                                         ;; We use the absolute value because
                                         ;; `ts-human-format-duration' returns 0 for
                                         ;; negative numbers.
                                         (- (hammy-current-duration hammy)
                                            (float-time (time-subtract (current-time)
                                                                       (hammy-current-interval-start-time hammy)))))))
                         (format "%s(%s%s:%s)"
                                 (hammy-name hammy)
                                 (if (hammy-overduep hammy)
                                     (propertize hammy-mode-lighter-overdue
                                                 'face 'hammy-mode-lighter-overdue)
                                   "")
                                 (propertize (hammy-interval-name (hammy-interval hammy))
                                             'face (hammy-interval-face (hammy-interval hammy)))
                                 (concat (if (hammy-overduep hammy)
                                             ;; We use the negative sign when
                                             ;; counting down to the end of an
                                             ;; interval (i.e. "T-minus...") .
                                             "+" "-")
                                         (ts-human-format-duration remaining 'abbr))))))
    (if hammy-active
        (concat (propertize hammy-mode-lighter-prefix
                            'face 'hammy-mode-lighter-prefix-active)
                ":"
                (mapconcat #'format-hammy hammy-active ",") " ")
      ;; No active hammys.
      (when hammy-mode-always-show-lighter
        (concat (propertize hammy-mode-lighter-prefix
                            'face 'hammy-mode-lighter-prefix-inactive)
                (if hammy-mode-lighter-suffix-inactive
                    (concat ":" hammy-mode-lighter-suffix-inactive))
                " ")))))

(defun hammy--mode-line-update (&rest _ignore)
  "Force updating of all mode lines when a hammy is active."
  (when hammy-active
    (force-mode-line-update 'all)))

;;;; Log buffer

(define-derived-mode hammy-log-mode read-only-mode "Hammy-Log"
  :interactive nil)

(progn
  (define-key hammy-log-mode-map "q" #'bury-buffer))

(defun hammy-view-log ()
  "Show Hammy log buffer."
  (interactive)
  (pop-to-buffer (hammy-log-buffer)))

(defun hammy-log-buffer ()
  "Return Hammy log buffer."
  (or (get-buffer hammy-log-buffer-name)
      (with-current-buffer (get-buffer-create hammy-log-buffer-name)
        (hammy-log-mode)
        (current-buffer))))

;;;; Notifications

(require 'notifications)

(defun hammy-notify (hammy &optional message)
  "Call `notifications-notify' for HAMMY with MESSAGE."
  (notifications-notify :title (format "Hammy (%s)"
                                       (hammy-name hammy))
                        :body (or message (hammy-format hammy))))

;;;; Hammys

;; Pre-defined for convenience.

(hammy-define "Flywheel"
  :documentation "Get your momentum going!"
  :intervals (list (interval :name "Rest"
                             :face 'font-lock-type-face
                             :duration "5 minutes"
                             :before (do (announce "Rest time!")
                                         (notify "Rest time!"))
                             :advance (do (announce "Rest time is over!")
                                          (notify "Rest time is over!")))
                   (interval :name "Work"
                             :face 'font-lock-builtin-face
                             :duration (climb "5 minutes" "45 minutes"
                                              :descend t :step "5 minutes")
                             :before (do (announce "Work time!")
                                         (notify "Work time!"))
                             :advance (do (announce "Work time is over!")
                                          (notify "Work time is over!"))))
  :after (do (announce "Flywheel session complete!")
             (notify "Flywheel session complete!"))
  :complete-p (do (and (> cycles 1)
                       interval
                       (equal "Work" interval-name)
                       (equal (duration "5 minutes") current-duration))))

(hammy-define "Move"
  :documentation "Don't forget to stretch your legs!"
  :intervals (list (interval :name "💺"
                             :duration "45 minutes"
                             :face 'font-lock-type-face
                             :before (do (announce "Whew!")
                                         (notify "Whew!"))
                             :advance (remind "10 minutes"
                                              (do (announce "Time to stretch your legs!")
                                                  (notify "Time to stretch your legs!"))))
                   (interval :name "🤸"
                             :duration "5 minutes"
                             :face 'font-lock-builtin-face
                             :before (do (announce "Move it!")
                                         (notify "Move it!"))
                             :advance (do (announce "Time for a sit-down...")
                                          (notify "Time for a sit-down...")))))

(hammy-define (propertize "🍅" 'face '(:foreground "tomato"))
  :documentation "The classic pomodoro timer."
  :intervals
  (list
   (interval :name "Working"
             :duration "25 minutes"
             :before (do (announce "Starting work time.")
                         (notify "Starting work time."))
             :advance (do (announce "Break time!")
                          (notify "Break time!")))
   (interval :name "Resting"
             :duration (do (if (and (not (zerop cycles))
                                    (zerop (mod cycles 3)))
                               ;; If a multiple of three cycles have
                               ;; elapsed, the fourth work period was
                               ;; just completed, so take a longer break.
                               "30 minutes"
                             "5 minutes"))
             :before (do (announce "Starting break time.")
                         (notify "Starting break time."))
             :advance (do (announce "Break time is over!")
                          (notify "Break time is over!")))))

(hammy-define "⅓-time"
  :documentation "Breaks that are ⅓ as long as the last work interval."
  :intervals
  (list
   (interval :name "Work"
             ;; It's intended that the user manually end this interval
             ;; when ready, but we specify a maximum of 90 minutes by
             ;; default.
             :duration "90 minutes"
             :before (do (announce "Starting work time (advance to break when ready).")
                         (notify "Starting work time (advance to break when ready)."))
             :advance (remind "10 minutes"
                              (do (let* ((current-duration (ts-human-format-duration
                                                            (float-time
                                                             (time-subtract (current-time) current-interval-start-time))))
                                         (message (format "You've worked for %s!" current-duration)))
                                    (announce message)
                                    (notify message)
                                    (when hammy-sound-end-work
                                      (play-sound-file hammy-sound-end-work))))))
   (interval :name "Break"
             :duration (do (pcase-let* ((`(,_interval ,start ,end) (car history))
                                        (work-seconds (float-time (time-subtract end start)))
                                        (duration (* work-seconds 0.33)))
                             (when (alist-get 'unused-break etc)
                               ;; Add unused break time.
                               (cl-incf duration (alist-get 'unused-break etc))
                               (setf (alist-get 'unused-break etc) nil))
                             duration))
             :before (do (let ((message (format "Starting break for %s."
                                                (ts-human-format-duration current-duration))))
                           (announce message)
                           (notify message)))
             :after (do (let* ((elapsed (float-time
                                         (time-subtract (current-time) current-interval-start-time)))
                               (unused (- current-duration elapsed)))
                          (when (> unused 0)
                            ;; "Bank" unused break time.
                            (if (alist-get 'unused-break etc)
                                (cl-incf (alist-get 'unused-break etc) unused)
                              (setf (alist-get 'unused-break etc) unused)))))
             :advance (remind "5 minutes"
                              (do (announce "Break time is over!")
                                  (notify "Break time is over!")
                                  (when hammy-sound-end-break
                                    (play-sound-file hammy-sound-end-break))))))
  :stopped (do (setf (alist-get 'unused-break etc) nil)))

;;;; Footer

(provide 'hammy)

;;; hammy.el ends here

;;; hammy.el --- Programmable, interactive interval timers  -*- lexical-binding: t; -*-

;; Copyright (C) 2022  Adam Porter

;; Author: Adam Porter <adam@alphapapa.net>
;; URL: https://github.com/alphapapa/hammy.el
;; Version: 0.3-pre
;; Package-Requires: ((emacs "28.1") (svg-lib "0.2.5") (ts "0.2.2"))
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

(require 'svg-lib)
(require 'ts)

;;;; Structs

(cl-defstruct ⏲
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
Called with one argument, the hammy.  Called when the ⏲'s
completion predicate returns non-nil.")
  (stopped nil :documentation "Function(s) called after stopping timer.
Called with one argument, the hammy.  Called by `🐹stop'.")
  (complete-p nil :documentation "Predicate that returns non-nil when ⏲ is complete.
Called with one argument, the hammy.  Called after each interval
is complete, before starting the next interval.")
  (overduep))

(cl-defstruct 🐹interval
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
advance until the user calls `🐹next'."))

(define-error '🐹complete "Hammy is over!")

;;;; Macros

;;;###autoload
(defmacro 🐹define (name &rest args)
  "Define a new Hammy named NAME made with ARGS.
Returns the ⏲, and adds ⏲ to `🐹hammys'.  NAME is a
string.  ARGS are passed to `make-hammy', which see.  Useful ones
include:

  `:documentation': An optional documentation string.

  `:intervals': A list of intervals.  Each one is defined with
    the local function `interval', which calls
    `make-🐹interval', which see for its arguments.

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

  `do (&rest body)': Expands to a lambda that binds `⏲' to
    the current ⏲ and evaluates BODY.  Within its BODY, these
    forms are bound:

    `current-duration': The duration in seconds of the current interval.
    `current-interval-start-time': The time at which the current interval began.
    `cycles': The number of cycles the ⏲ has completed.
    `etc': The ⏲'s `etc' slot.
    `history': The ⏲'s history list.
    `interval': The current interval (a `🐹interval' struct).
    `interval-name': The name of the current interval.

  `elapsed (&optional interval)': Calls `🐹elapsed' with the
    ⏲, which see.

  `interval (&rest args)': Calls `make-🐹interval', which
    see.

  `num-intervals ()': Returns the ⏲'s number of intervals.

  `remind (delay &rest fns)': Return a function that is called
    every DELAY seconds until the interval is manually advanced,
    calling FNS each time.  (The function automatically makes
    necessary adjustments to the ⏲ to set and cancel the
    periodic reminders.)

  `run (command)': Runs COMMAND (a string) asynchronously with
    `make-process', discarding its output and return value."
  (declare (indent defun))
  ;; In some ways, it might be preferable for this macro to expand to
  ;; the ⏲ struct, but then it wouldn't be forward-compatible if
  ;; the structure changes, so we just expand to the code that makes
  ;; the struct.

  ;; NOTE: This macro essentially expands to a call to `make-hammy'
  ;; with the given arguments, wrapping some of them in macrolet,
  ;; symbol-macrolet, and labels forms, and lambdas.  This way, we get
  ;; some checking for free at macro expansion time and compilation
  ;; time, rather than those errors happening at runtime.

  ;; NOTE: If a user byte-compiles a config file containing a
  ;; `🐹define' call, and the definition of this macro changes in
  ;; a later version, it will be a problem.  So it would be nice if
  ;; this were a function instead of a macro, but that would mean that
  ;; the user would have to quote the argument to prevent evaluation,
  ;; which would likely be confusing to many users.  So, for now, at
  ;; least, it will be a macro.
  `(cl-macrolet ((announce (message)
                   `(🐹announce ⏲ ,message))
                 (notify (message)
                   `(🐹notify ⏲ ,message))
                 (cycles ()
                   `(🐹cycles ⏲))
                 (listify (place)
                   `(unless (listp ,place)
                      (setf ,place (list ,place))))
                 (listify-functions (place)
                   `(when (functionp ,place)
                      (setf ,place (list ,place))))
                 (do (&rest body)
                   `(lambda (⏲)
                      (cl-symbol-macrolet ((current-duration (🐹current-duration ⏲))
                                           (current-interval-start-time (🐹current-interval-start-time ⏲))
                                           (cycles (🐹cycles ⏲))
                                           (etc (🐹etc ⏲))
                                           (history (🐹history ⏲))
                                           (interval (🐹interval ⏲))
                                           (interval-name (🐹interval-name interval)))
                        (ignore ⏲)
                        ,@body))))
     ;; NOTE: Some of these functions are called at "⏲ time" (I
     ;; know...), while others return lambdas to be called at ⏲
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
                   (apply #'make-🐹interval args))
                 (elapsed (⏲ &optional interval)
                   "Call `🐹elapsed', which see."
                   (🐹elapsed ⏲ interval))
                 (num-intervals (⏲)
                   (ring-length (🐹intervals ⏲)))
                 (history (⏲)
                   (🐹history ⏲))
                 (cycles (⏲)
                   (🐹cycles ⏲))
                 (climb (from to &key descend step)
                   (lambda (⏲)
                     (let* ((from (duration from))
                            (to (duration to))
                            (apex (/ to from))
                            (step (cl-typecase step
                                    (string (duration step))
                                    (number step)))
                            (duration (cl-labels
                                          ((ascend
                                             () (min (* (pcase (cycles ⏲)
                                                          (0 1)
                                                          (height (1+ height)))
                                                        from)
                                                     to))
                                           (descend
                                             () (min (* (pcase (- (* 2 apex) (cycles ⏲))
                                                          (0 1)
                                                          (height (1- height)))
                                                        from)
                                                     to)))
                                        (if (< (cycles ⏲) apex)
                                            ;; Spin up!
                                            (if step
                                                (+ from (* step (cycles ⏲)))
                                              (ascend))
                                          ;; Spin down...
                                          (pcase-exhaustive descend
                                            (`nil (ascend))
                                            (`t (🐹log ⏲
                                                       (format "Descending... (Cycles:%s  Apex:%s  From:%s  To:%s  Step:%s"
                                                               (cycles ⏲) apex from to step))
                                                (if step
                                                    (+ from (* step (- (* 2 apex) (cycles ⏲))))
                                                  (descend))))))))
                       duration)))
                 (remind (delay &rest fns)
                   (lambda (⏲)
                     (listify (🐹after ⏲))
                     (cl-pushnew #'cancel-reminder (🐹after ⏲))
                     (dolist (fn fns)
                       (funcall fn ⏲))
                     (let ((delay-secs (duration delay)))
                       ;; TODO: Allow the duration to also be a function to return the reminder delay.
                       (setf (alist-get 'reminder (🐹etc ⏲))
                             (run-with-timer delay-secs delay-secs
                                             (lambda (⏲)
                                               (dolist (fn fns)
                                                 (funcall fn ⏲)))
                                             ⏲)))
                     ;; TODO: Might need to cancel and restart
                     ;; the reminder when a ⏲ is paused,
                     ;; too.
                     (listify (🐹interval-after (🐹interval ⏲)))
                     (cl-pushnew #'cancel-reminder (🐹interval-after (🐹interval ⏲)))))
                 (cancel-reminder (⏲)
                   (when (alist-get 'reminder (🐹etc ⏲))
                     (cancel-timer (alist-get 'reminder (🐹etc ⏲)))
                     (setf (alist-get 'reminder (🐹etc ⏲)) nil))))
       (let* ((⏲ (make-hammy :name ,name ,@args))
              (ring (make-ring (length (🐹intervals ⏲)))))
         (dolist (interval (🐹intervals ⏲))
           (progn
             ;; Ensure that function slots are lists of functions, not just a function.
             (listify-functions (🐹interval-before interval))
             (listify-functions (🐹interval-after interval))
             (listify-functions (🐹interval-advance interval)))
           (ring-insert-at-beginning ring interval))
         (setf (🐹intervals ⏲) ring)
         (setf 🐹hammys (cl-delete ,name 🐹hammys :test #'equal :key #'🐹name))
         (push ⏲ 🐹hammys)
         ⏲))))

(defun 🐹call (fn-or-fns &rest args)
  "Call FN-OR-FNS with ARGS.
If FN-OR-FNS is a function, call it; if a list of functions, call
each of them; if nil, do nothing."
  (cl-typecase fn-or-fns
    (null nil)
    (function (apply fn-or-fns args))
    (list (dolist (fn fn-or-fns)
            (apply fn args)))))

;;;; Inline functions

(defsubst 🐹-current-interval-elapsed (⏲)
  "Return elapsed seconds in HAMMY's current interval."
  (float-time (time-subtract (current-time) (🐹current-interval-start-time ⏲))))

(defsubst 🐹-current-interval-remaining (⏲)
  "Return remaining seconds in HAMMY's current interval."
  ;; TODO: Use this in more places, probably.
  (- (🐹current-duration ⏲) (🐹-current-interval-elapsed ⏲)))

;;;; Variables

(defvar org-clock-hd-marker)

(defvar 🐹hammys nil
  "List of defined hammys.
Define a ⏲ with `🐹define'.")

(defvar 🐹active nil
  "List of active hammys.")

;;;; Customization

(defgroup ⏲ nil
  "Programmable interval timers."
  :group 'convenience)

(defcustom 🐹log-buffer-name "*Hammy Log*"
  "Name of Hammy log buffer."
  :type 'string)

(defcustom 🐹start-hook '((lambda (⏲) (🐹log ⏲ "Starting...")))
  "Functions run when a ⏲ is started.
Called with the hammy."
  :type 'hook)

(defcustom 🐹stopped '((lambda (⏲)
                             (🐹log ⏲ "Stopped.")
                             (🐹log ⏲ (🐹summary ⏲))))
  "Functions run when a ⏲ is stopped.
Called with the hammy."
  :type 'hook)

(defcustom 🐹complete-hook '((lambda (⏲)
                                   (🐹log ⏲ "Completed.")
                                   (🐹log ⏲ (🐹summary ⏲))))
  "Functions run when a ⏲ is completed.
That is, when the ⏲ completes its programmed cycles (not when
manually interrupted).  Called with the ⏲, and optionally a
message."
  :type 'hook)

(defcustom 🐹interval-hook '((lambda (⏲ &optional message) (🐹log ⏲ message)))
  "Functions run when a ⏲ completes an interval.
Called with the ⏲, and optionally a message."
  :type 'hook)

(defcustom 🐹cycle-hook '((lambda (⏲) (🐹log ⏲ "Cycled.")))
  "Functions run when a ⏲ completes a cycle.
Called with the ⏲, and optionally a message."
  :type 'hook)

(defcustom 🐹sound-end-break nil
  "Play this sound when a break interval ends."
  :type '(choice file (const :tag "No sound" nil)))

(defcustom 🐹sound-end-work nil
  "Play this sound when a work interval ends."
  :type '(choice file (const :tag "No sound" nil)))

;;;; Commands

(defun 🐹adjust (⏲)
  "Adjust HAMMY's interval durations."
  (interactive (list (🐹complete "Adjust hammy: " 🐹hammys)))
  ;; TODO: Reset durations in `🐹reset'.
  (cl-labels
      ((adjust-interval (interval)
         (cl-symbol-macrolet
             ((original-duration
                (alist-get interval (alist-get 'original-durations (🐹etc ⏲)))))
           (let* ((old-duration (🐹interval-duration interval))
                  (input-duration
                   (read-string
                    (format "New duration (number, function, or quoted-string duration) for interval \"%s\": "
                            (🐹interval-name interval))
                    nil nil (prin1-to-string old-duration)))
                  (new-duration
                   (unless (string-empty-p input-duration)
                     ;; TODO: Allow the user to type, e.g. "25 minutes" without enclosing quotes.
                     (car (read-from-string input-duration)))))
             (when new-duration
               (cl-check-type new-duration (or number function string))
               (unless original-duration
                 ;; Only save the original duration the first time the interval is adjusted.
                 (setf original-duration old-duration))
               (setf (🐹interval-duration interval) new-duration))))))
    (mapc #'adjust-interval (ring-elements (🐹intervals ⏲)))))

;;;###autoload
(cl-defun 🐹start (⏲ &key duration interval)
  "Start HAMMY and return it.
If DURATION, set its first interval to last that many seconds.
INTERVAL may be an interval in the ⏲ to start
with (interactively, with universal prefix, prompt for the
interval with completion)."
  (interactive
   (let ((⏲ (🐹complete "Start hammy: " (cl-remove-if #'🐹timer 🐹hammys))))
     (list ⏲
           :duration (cl-typecase current-prefix-arg
                       (number current-prefix-arg))
           :interval (cl-typecase current-prefix-arg
                       (null nil)
                       (list (🐹complete-interval ⏲ :prompt "Start with interval: "))))))
  (when (map-elt (🐹etc ⏲) 'pausedp)
    (user-error "Hammy paused: %s  (Use `🐹toggle' to resume.)" (🐹format ⏲)))
  (when (🐹interval ⏲)
    (user-error "Hammy already started: %s" (🐹format ⏲)))
  (run-hook-with-args '🐹start-hook ⏲)
  (🐹call (🐹before ⏲) ⏲)
  (🐹next ⏲ :duration duration :advance t :interval interval)
  (push ⏲ 🐹active)
  ⏲)

(declare-function org-before-first-heading-p "org")
;;;###autoload
(defun 🐹start-org-clock-in (&rest _ignore)
  "Call `org-clock-in' and start a ⏲ (or use an already-started one).
If point is in an Org entry, clock into it; otherwise, offer a
list of recently clocked tasks to clock into.  The Org task will
then automatically be clocked out during the ⏲'s second
interval (and when the ⏲ is stopped), and back in when the
first interval resumes.  (If the user clocks into a different
task while the ⏲ is running, the task that is clocked-in when
the work interval ends will be clocked back into when the next
work interval begins.)

Returns the ⏲ from `🐹start'.  Assumes that the ⏲'s
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
  (let ((⏲ (🐹complete "Clock in with Hammy: " 🐹hammys)))
    (unless (🐹interval ⏲)
      (🐹start ⏲))
    (cl-macrolet ((pushfn (fn place)
                    `(cl-pushnew ,fn ,place :test #'equal)))
      (pushfn #'🐹-org-clock-in (🐹interval-before (🐹interval ⏲)))
      (pushfn #'🐹-org-clock-out (🐹interval-after (🐹interval ⏲)))
      (pushfn #'🐹-org-clock-out (🐹stopped ⏲)))
    ⏲))

(defun 🐹stop (⏲ &optional quietly)
  "Stop HAMMY timer.
If QUIETLY, don't say so."
  (interactive
   (list (or (🐹complete "Stop hammy: " 🐹active)
             (user-error "No active hammys"))))
  (pcase-let* (((cl-struct ⏲ (timer internal-timer)
                           (etc (map reminder)))
                ⏲)
               ;; TODO: Logging, totals, etc.
               (message "Stopped."))
    (when internal-timer
      (cancel-timer internal-timer)
      (setf (🐹timer ⏲) nil)
      (🐹log ⏲ message)
      (unless quietly
        (message message)))
    (when reminder
      (cancel-timer reminder)
      (setf (alist-get 'reminder (🐹etc ⏲)) nil))
    ;; Run the hook after having stopped the ⏲, so any errors in
    ;; stopped functions won't prevent the ⏲ from stopping
    ;; correctly; and do it before resetting the ⏲, so functions
    ;; in the stop hook can access the ⏲'s data before resetting.
    (🐹-record-interval ⏲)
    (run-hook-with-args '🐹stopped ⏲)
    (🐹call (🐹stopped ⏲) ⏲)
    (setf (🐹interval ⏲) nil
          🐹active (remove ⏲ 🐹active))
    ⏲))

(cl-defun 🐹next (⏲ &key duration advance interval)
  "Advance to HAMMY's next interval.
If DURATION (interactively, with numeric prefix), set the
interval's duration to DURATION seconds.  If ADVANCE, advance to
the next interval even if the previous interval has an
unsatisfied ADVANCE predicate.  INTERVAL may be an interval in
the ⏲ to advance to (interactively, with universal prefix,
prompt for the interval with completion)."
  (interactive
   (if-let ((⏲ (🐹complete "Advance hammy: " 🐹active)))
       (list ⏲
             :duration (cl-typecase current-prefix-arg
                         (number current-prefix-arg))
             :advance t
             :interval (cl-typecase current-prefix-arg
                         (null nil)
                         (list (🐹complete-interval ⏲ :prompt "Advance to interval: "))))
     (user-error (substitute-command-keys "No active hammys (use \"\\[🐹start]\")"))))
  (when (🐹timer ⏲)
    ;; Cancel any outstanding timer.
    (cancel-timer (🐹timer ⏲))
    (setf (🐹timer ⏲) nil))
  (cl-labels ((advancep ()
                (or (and (🐹interval ⏲)
                         (eq 'auto (🐹interval-advance (🐹interval ⏲))))
                    advance)))
    (when (🐹interval ⏲)
      ;; Hammy already started, interval completed (or ready to be
      ;; advanced).
      (when (and (advancep)
                 (equal (🐹interval ⏲)
                        (ring-ref (🐹intervals ⏲)
                                  (1- (ring-length (🐹intervals ⏲))))))
        ;; Cycle completed.
        (cl-incf (🐹cycles ⏲))
        ;; TODO: Not sure if it makes sense to run the cycle hook here
        ;; or later, after running other hooks.
        (run-hook-with-args '🐹cycle-hook ⏲)))
    (if (and (advancep)
             (🐹complete-p ⏲)
             (funcall (🐹complete-p ⏲) ⏲))
        ;; Hammy complete.
        (progn
          (🐹stop ⏲ 'quietly)
          (run-hook-with-args '🐹complete-hook ⏲)
          (🐹call (🐹after ⏲) ⏲))
      ;; Hammy not complete: start next interval.
      (pcase-let* (((cl-struct ⏲ (interval current-interval)) ⏲)
                   (next-interval (or interval
                                      (if current-interval
                                          (ring-next (🐹intervals ⏲) current-interval)
                                        (ring-ref (🐹intervals ⏲) 0))))
                   (next-duration))
        (if (not (advancep))
            ;; Interval requires manual advancing.
            (progn
              (🐹log ⏲ "Waiting for user to advance...")
              (setf (🐹overduep ⏲) t)
              (🐹call (🐹interval-advance (🐹interval ⏲)) ⏲))
          ;; Automatically advancing, manually advancing, or starting the hammy.
          (when (🐹interval ⏲)
            ;; Advancing to the next interval (rather than starting the ⏲).
            ;; NOTE: We call the interval-hook and the interval's after
            ;; functions when actually advancing to the next interval.
            (🐹-record-interval ⏲)
            (🐹log ⏲ (format "Elapsed: %s" (🐹format-current-times ⏲)))
            (run-hook-with-args '🐹interval-hook ⏲
                                (format "Interval ended: %s"
                                        (🐹interval-name (🐹interval ⏲))))
            (🐹call (🐹interval-after (🐹interval ⏲)) ⏲))
          (setf (🐹interval ⏲) next-interval
                (🐹current-interval-start-time ⏲) (current-time)
                ;; We calculate the next duration after recording the
                ;; previous interval so, e.g. the ⅓-time ⏲ can
                ;; refer to its duration.
                next-duration (or duration
                                  (pcase-exhaustive
                                      (cl-etypecase (🐹interval-duration next-interval)
                                        (function (condition-case _err
                                                      (funcall (🐹interval-duration next-interval) ⏲)
                                                    (🐹complete
                                                     (run-hook-with-args '🐹complete-hook ⏲)
                                                     (message "Hammy is over!  (%s)" (🐹name ⏲))
                                                     nil)))
                                        ((or number string) (🐹interval-duration next-interval)))
                                    ((and (pred numberp) it) it)
                                    ((and (pred stringp) it) (timer-duration it))))
                (🐹current-duration ⏲) next-duration
                (🐹overduep ⏲) nil)
          (when next-duration
            ;; Starting next interval.
            (🐹call (🐹interval-before next-interval) ⏲)
            ;; TODO: Mention elapsed time of just-completed interval.
            (run-hook-with-args '🐹interval-hook ⏲
                                (format "Interval started: %s (%s)"
                                        (🐹interval-name (🐹interval ⏲))
                                        (ts-human-format-duration (🐹current-duration ⏲)
                                                                  'abbr)))
            (setf (🐹timer ⏲) (run-at-time next-duration nil #'🐹next ⏲)))))))
  ⏲)

(defun 🐹reset (⏲)
  "Reset HAMMY timer.
If already running, restarts it."
  (interactive (list (🐹complete "Reset hammy: " 🐹hammys)))
  (let ((runningp (🐹timer ⏲)))
    (when runningp
      (🐹stop ⏲ 'quietly))
    (setf (🐹cycles ⏲) 0
          (🐹etc ⏲) nil
          (🐹history ⏲) nil
          (🐹interval ⏲) nil
          (🐹current-interval-start-time ⏲) nil
          (🐹overduep ⏲) nil)
    (when (alist-get 'original-durations (🐹etc ⏲))
      ;; Restore any original durations.
      (cl-loop for (interval . duration) in (alist-get 'original-durations (🐹etc ⏲))
               do (progn
                    (setf (🐹interval-duration interval) duration)
                    (cl-callf2 assoc-delete-all 'original-durations (🐹etc ⏲)))))
    (when runningp
      (🐹start ⏲))
    ⏲))

(defun 🐹toggle (⏲)
  "Toggle HAMMY.
If running, pause it; if paused, resume it.

Pausing records the current interval and remaining time and calls
`🐹stop'.  Resuming calls `🐹start' with the recorded
interval and remaining time."
  (interactive
   (list (🐹complete "Toggle hammy: "
                         (append (cl-remove-if-not (lambda (⏲)
                                                     (map-elt (🐹etc ⏲) 'pausedp))
                                                   🐹hammys)
                                 🐹active))))
  ;; Using `τ' as a prefix for place symbols ("τόπος" meaning "place").
  (cl-symbol-macrolet ((τpausedp (map-elt (🐹etc ⏲) 'pausedp))
                       (τlast-remaining (map-elt (🐹etc ⏲) 'last-remaining))
                       (τlast-interval (map-elt (🐹etc ⏲) 'last-interval)))
    (if (not τpausedp)
        (let ((elapsed (🐹-current-interval-elapsed ⏲))
              (remaining (🐹-current-interval-remaining ⏲))
              (interval (🐹interval ⏲)))
          (setf τpausedp t
                τlast-remaining remaining
                τlast-interval interval)
          (🐹stop ⏲ 'quietly)
          (🐹log ⏲ (format "Paused after %.0f seconds.  %.0f seconds remaining in interval %S."
                                   elapsed remaining (🐹interval-name interval))))
      (let ((remaining τlast-remaining)
            (interval τlast-interval))
        (setf τpausedp nil
              τlast-remaining nil
              τlast-interval nil)
        (🐹start ⏲ :interval interval :duration remaining))))
  ⏲)

;;;; Functions

(cl-defun 🐹complete-interval (⏲ &key (prompt "Interval: "))
  "Return an interval selected in HAMMY with completion.
PROMPT may be specified."
  (let* ((intervals (ring-elements (🐹intervals ⏲)))
         (names (mapcar #'🐹interval-name intervals))
         (selected-name (completing-read prompt names nil t)))
    (cl-find selected-name intervals
             :test (lambda (name interval)
                     (equal name (🐹interval-name interval))))))

(defun 🐹format (⏲ &optional message)
  "Return formatted status for HAMMY, optionally with MESSAGE."
  (let* ((interval (cond ((🐹interval ⏲)
                          (format "%s (%s)"
                                  (🐹interval-name (🐹interval ⏲))
                                  (ts-human-format-duration (🐹current-duration ⏲) 'abbr)))
                         ((and (🐹complete-p ⏲)
                               (funcall (🐹complete-p ⏲) ⏲))
                          "Completed.")
                         (t
                          "None")))
         (message (if message (format "  Message:%S" message) "")))
    (format "Hammy (%s): Interval:%s  Cycles:%s%s"
            (🐹name ⏲) interval (🐹cycles ⏲) message)))

(defun 🐹log (⏲ &optional message)
  "Log MESSAGE for HAMMY to log buffer."
  (with-current-buffer (🐹log-buffer)
    (let* ((inhibit-read-only t)
           (buffer-window (get-buffer-window (current-buffer)))
           (point-at-eob-p (equal (point-max)
                                  (if buffer-window
                                      (window-point buffer-window)
                                    (point)))))
      (save-excursion
        (goto-char (point-max))
        (insert (format-time-string "%Y-%m-%d %H:%M:%S  ") (🐹format ⏲ message) "\n"))
      (when point-at-eob-p
        (if buffer-window
            (setf (window-point buffer-window) (point-max))
          (setf (point) (point-max)))))))

(defun 🐹format-current-times (⏲)
  "Return current times for HAMMY formatted.
String includes elapsed time of the current interval and any
overrun time."
  (let* ((elapsed-secs (float-time
                        (time-subtract (current-time)
                                       (🐹current-interval-start-time ⏲))))
         (difference (float-time
                      (time-subtract (🐹current-duration ⏲) elapsed-secs))))
    (format "%s%s"
            (ts-human-format-duration elapsed-secs 'abbr)
            (if (not (zerop difference))
                (let ((sign (if (< 0 difference) "-" "+")))
                  (format " (%s%s)" sign (ts-human-format-duration (abs difference) 'abbr)))
              ""))))

(defun 🐹elapsed (⏲ &optional interval)
  "Return HAMMY's elapsed time in seconds.
If INTERVAL (an interval struct or an interval name string),
return the elapsed time for that interval (summed across all
cycles)."
  (pcase-let* (((cl-struct ⏲ history) ⏲)
               (interval-history
                (cl-typecase interval
                  (🐹interval (cl-remove-if-not (lambda (element)
                                                      (eq interval (car element)))
                                                    history))
                  (string (cl-remove-if-not (lambda (element)
                                              (equal interval (🐹interval-name (car element))))
                                            history))
                  (t history))))
    (cl-loop for (_interval start-time end-time) in interval-history
             sum (float-time (time-subtract end-time start-time)))))

(defun 🐹complete (prompt hammys)
  "Return one of HAMMYS selected with completion and PROMPT."
  (cl-labels ((describe (⏲)
                (format "%s (%s)"
                        (🐹name ⏲)
                        (🐹documentation ⏲))))
    (pcase (length hammys)
      (0 nil)
      (1 (car hammys))
      (_ (let* ((map (cl-loop for ⏲ in hammys
                              collect (cons (describe ⏲) ⏲)))
                (description (completing-read prompt map nil t)))
           (alist-get description map nil nil #'equal))))))

(defun 🐹announce (⏲ message)
  "Announce MESSAGE in the echo area for HAMMY."
  (message "Hammy (%s): %s"
           (🐹name ⏲) message))

(defun 🐹summary (⏲)
  "Return a summary string for HAMMY.
Summary includes elapsed times, etc."
  (format "Total elapsed:%s  Intervals:%s  Cycles:%s"
          (ts-human-format-duration (🐹elapsed ⏲) 'abbr)
          (mapconcat (lambda (interval)
                       (format "(%s:%s)"
                               (🐹interval-name interval)
                               (ts-human-format-duration (🐹elapsed ⏲ interval) 'abbr)))
                     (ring-elements (🐹intervals ⏲))
                     "")
          (🐹cycles ⏲)))

(declare-function org-clock-in "org-clock")
(defun 🐹-org-clock-in (⏲)
  "Clock in to HAMMY's Org task."
  (cl-symbol-macrolet ((marker (alist-get 'org-clock-hd-marker (🐹etc ⏲))))
    (when marker
      (org-with-point-at marker
        (org-clock-in))
      ;; Unset the saved marker, because it will be saved again when
      ;; clocking out.
      (setf marker nil))))

(declare-function org-clocking-p "org-clock")
(declare-function org-clock-out "org-clock")
(defun 🐹-org-clock-out (⏲)
  "Clock out of HAMMY's Org task."
  (when (org-clocking-p)
    ;; Record the clocked-in task so we can clock back in to it later.
    ;; `org-clock-out' kills the marker, so we have to copy it for
    ;; future reference.
    (setf (alist-get 'org-clock-hd-marker (🐹etc ⏲))
          (copy-marker org-clock-hd-marker))
    (org-clock-out)))

(defun 🐹-record-interval (⏲)
  "Record current interval in HAMMY's history."
  (push (list (🐹interval ⏲)
              (🐹current-interval-start-time ⏲)
              (current-time))
        (🐹history ⏲)))

;;;; Mode

(defcustom 🐹mode-always-show-lighter t
  "Show lighter even when no hammys are running."
  :type 'boolean)

(defcustom 🐹mode-lighter-prefix "🐹"
  "Show lighter even when no hammys are running."
  :type 'string)

(defcustom 🐹mode-lighter-overdue "!"
  "Shown when ⏲ is due to be manually advanced."
  :type 'string)

(defcustom 🐹mode-lighter-suffix-inactive "∅"
  "Shown when no hammys are running."
  :type 'string)

(defcustom 🐹mode-lighter-pie t
  "Show progress pie in the lighter."
  :type 'boolean)

(defcustom 🐹mode-lighter-pie-update-interval 10
  "Update a ⏲'s pie every this many seconds."
  :type 'integer)

(defcustom 🐹mode-update-mode-line-continuously t
  "Update the mode line every second while a ⏲ is running."
  :type 'boolean)

(defface 🐹mode-lighter-prefix-inactive '((t (:inherit warning)))
  "Used when no ⏲ is active.")

(defface 🐹mode-lighter-prefix-active '((t (:inherit font-lock-type-face)))
  "Used when no ⏲ is active.")

(defface 🐹mode-lighter-overdue '((t (:inherit error)))
  "Used when no ⏲ is active.")

(defvar 🐹mode-update-mode-line-timer nil
  "Timer used to update the mode line.")

(defface 🐹mode-lighter-pie '((t (:inherit mode-line)))
  "Hammy progress pies.
If showing progress in the mode line or tab bar, inherit from the
appropriate face to ensure proper appearance.")

(defface 🐹mode-lighter-pie-normal '((t (:inherit 🐹mode-lighter-pie)))
  "Hammy with > 50% remaining.")

(defface 🐹mode-lighter-pie-50 '((t (:inherit 🐹mode-lighter-pie)))
  "Hammy with <= 50% remaining.")

(defface 🐹mode-lighter-pie-25 '((t (:inherit font-lock-variable-name-face)))
  "Hammy with <= 25% remaining.")

(defface 🐹mode-lighter-pie-10 '((t (:inherit  font-lock-warning-face)))
  "Hammy with <= 10% remaining.")

(defface 🐹mode-lighter-pie-0 '((t (:inherit error)))
  "Hammy that is overdue.")

;;;###autoload
(define-minor-mode 🐹mode
  "Show active ⏲ in the mode line."
  :global t
  (let ((lighter '(🐹mode (:eval (🐹mode-lighter)))))
    (if 🐹mode
        (progn
          (when 🐹mode-update-mode-line-continuously
            ;; TODO: Only run this timer when a ⏲ is running.
            (when (timerp 🐹mode-update-mode-line-timer)
              ;; Cancel any existing timer.  Generally shouldn't happen, but not impossible.
              (cancel-timer 🐹mode-update-mode-line-timer))
            (setf 🐹mode-update-mode-line-timer (run-with-timer 1 1 #'🐹-mode-line-update)))
          (add-hook '🐹interval-hook #'🐹-mode-line-update)
          ;; Avoid adding the lighter multiple times if the mode is activated again.
          (cl-pushnew lighter global-mode-string :test #'equal))
      (when 🐹mode-update-mode-line-timer
        (cancel-timer 🐹mode-update-mode-line-timer)
        (setf 🐹mode-update-mode-line-timer nil))
      (remove-hook '🐹interval-hook #'🐹-mode-line-update)
      (setf global-mode-string
            (remove lighter global-mode-string)))))

(defun 🐹mode-lighter ()
  "Return the mode-line lighter for `🐹mode'."
  (cl-labels
      ((format-hammy (⏲)
         (let ((remaining
                (abs
                 ;; We use the absolute value because `ts-human-format-duration'
                 ;; returns 0 for negative numbers.
                 (- (🐹current-duration ⏲)
                    (float-time (time-subtract (current-time)
                                               (🐹current-interval-start-time ⏲)))))))
           (format "%s(%s%s:%s)"
                   (🐹name ⏲)
                   (if (🐹overduep ⏲)
                       (propertize 🐹mode-lighter-overdue
                                   'face '🐹mode-lighter-overdue)
                     "")
                   (propertize (🐹interval-name (🐹interval ⏲))
                               'face (🐹interval-face (🐹interval ⏲)))
                   (concat (when 🐹mode-lighter-pie
                             (propertize " " 'display (🐹-pie ⏲)))
                           (if (🐹overduep ⏲)
                               ;; We use the negative sign when counting down to
                               ;; the end of an interval (i.e. "T-minus...") .
                               "+" "-")
                           (ts-human-format-duration remaining 'abbr))))))
    (if 🐹active
        (concat (propertize 🐹mode-lighter-prefix
                            'face '🐹mode-lighter-prefix-active)
                ":"
                (mapconcat #'format-hammy 🐹active ",") " ")
      ;; No active hammys.
      (when 🐹mode-always-show-lighter
        (concat (propertize 🐹mode-lighter-prefix
                            'face '🐹mode-lighter-prefix-inactive)
                (if 🐹mode-lighter-suffix-inactive
                    (concat ":" 🐹mode-lighter-suffix-inactive))
                " ")))))

(defun 🐹status ()
  "Show the status of any active hammys in the echo area."
  (interactive)
  (message "%s"
           (mapconcat (lambda (⏲)
                        (concat (🐹format ⏲)
                                "  Elapsed:" (🐹format-current-times ⏲) ""))
                      🐹active "\n")))

(defun 🐹-mode-line-update (&rest _ignore)
  "Force updating of all mode lines when a ⏲ is active."
  (when 🐹active
    (force-mode-line-update 'all)))

(defun 🐹-pie (⏲)
  "Return HAMMY's pie, updating it if necessary."
  ;; This function is carefully designed and tested to not make more pie than
  ;; necessary (because the mode line, header line, tab bar, etc. are updated
  ;; more often than one would expect).  And the unusual construction is
  ;; designed to minimize the number of times the `τpie' is accessed
  ;; (which requires type-checking the struct each time).
  (cl-symbol-macrolet
      ((τpie (alist-get 'pie (🐹etc ⏲)))
       (τlast-pie-elapsed (alist-get 'last-pie-elapsed (🐹etc ⏲))))
    (let* (🥧
           (elapsed (floor (🐹-current-interval-elapsed ⏲)))
           (update-pie-p (or (and (not (equal elapsed τlast-pie-elapsed))
                                  (zerop (mod elapsed 🐹mode-lighter-pie-update-interval)))
                             (not (setf 🥧 τpie)))))
      (when update-pie-p
        (setf 🥧 (setf τlast-pie-elapsed elapsed
                       τpie (🐹-make-pie ⏲))))
      🥧)))

(defun 🐹-make-pie (⏲)
  "Return an SVG progress pie for HAMMY.
Suitable for inserting with `insert-image'."
  (let* ((elapsed (🐹-current-interval-elapsed ⏲))
         (remaining (- (🐹current-duration ⏲) elapsed))
         (fraction (/ remaining (🐹current-duration ⏲)))
         (face (pcase fraction
                 ((pred (< 0.50)) '🐹mode-lighter-pie-normal)
                 ((pred (< 0.25)) '🐹mode-lighter-pie-50)
                 ((pred (< 0.10)) '🐹mode-lighter-pie-25)
                 ((pred (< 0.00)) '🐹mode-lighter-pie-10)
                 (_ '🐹mode-lighter-pie-0))))
    ;; After choosing face, pass the absolute value of the fraction so
    ;; it will fill up again as it becomes further overdue.
    (svg-lib-progress-pie (abs fraction) nil :height 1.0
                          :background (face-attribute '🐹mode-lighter-pie :background nil t)
                          :foreground (face-attribute face :foreground nil t))))

;;;; Log buffer

(define-derived-mode 🐹log-mode read-only-mode "🐹Log"
  :interactive nil)

(progn
  (define-key 🐹log-mode-map "q" #'bury-buffer))

(defun 🐹view-log ()
  "Show Hammy log buffer."
  (interactive)
  (pop-to-buffer (🐹log-buffer)))

(defun 🐹log-buffer ()
  "Return Hammy log buffer."
  (or (get-buffer 🐹log-buffer-name)
      (with-current-buffer (get-buffer-create 🐹log-buffer-name)
        (🐹log-mode)
        (current-buffer))))

;;;; Notifications

(require 'notifications)

(defun 🐹notify (⏲ &optional message)
  "Call `notifications-notify' for HAMMY with MESSAGE."
  (notifications-notify :title (format "Hammy (%s)"
                                       (🐹name ⏲))
                        :body (or message (🐹format ⏲))))

;;;; Hammys

;; Pre-defined for convenience.

(🐹define "Flywheel"
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

(🐹define "Move"
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

(🐹define (propertize "🍅" 'face '(:foreground "tomato"))
  :documentation "The classic pomodoro timer."
  :intervals
  (list
   (interval :name "Working"
             :duration "25 minutes"
             :before (do (announce "Starting work time.")
                         (notify "Starting work time."))
             :advance (remind "10 minutes"
                              (do (announce "Break time!")
                                  (notify "Break time!"))))
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
             :advance (remind "10 minutes"
                              (do (announce "Break time is over!")
                                  (notify "Break time is over!"))))))

(🐹define "⅓-time"
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
                              (do (let* ((current-duration
                                          (ts-human-format-duration
                                           (float-time
                                            (time-subtract (current-time)
                                                           current-interval-start-time))))
                                         (message (format "You've worked for %s!" current-duration)))
                                    (announce message)
                                    (notify message)
                                    (when 🐹sound-end-work
                                      (play-sound-file 🐹sound-end-work))))))
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
             :after (do (let* ((elapsed
                                (float-time
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
                                  (when 🐹sound-end-break
                                    (play-sound-file 🐹sound-end-break))))))
  :stopped (do (setf (alist-get 'unused-break etc) nil)))

;;;; Footer

(provide '⏲)

;;; hammy.el ends here

;; Local Variables:
;; read-symbol-shorthands: (("🐹" . "hammy-"))
;; End:

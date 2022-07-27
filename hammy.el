;;; hammy.el --- Programmable, periodic timers for working and taking breaks  -*- lexical-binding: t; -*-

;; Copyright (C) 2022  Adam Porter

;; Author: Adam Porter <adam@alphapapa.net>
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

;; TODO: Sentinels to stop timer after so many cycles or upon other conditions.
;; TODO: Logging.
;; TODO: Pausing.
;; TODO: Saving and choosing timers (maybe using Emacs bookmarks?).
;; TODO: Mode-line lighter, etc.

;;; Code:

;;;; Requirements

(require 'cl-lib)

;;;; Structs

(cl-defstruct hammy-hammy
  (name "" :type 'string) (documentation "" :documentation "Documentation.")
  (elapsed nil :documentation "List of elapsed intervals."
           ;; FIXME: This.
           )
  (cycles 0 :documentation "Number of times the timer has gone through a cycle of all intervals.")
  (intervals nil :documentation "List of defined intervals.")
  (current-interval) (current-duration)
  (last-duration nil :documentation "Length in seconds of last interval.")
  (timer nil :documentation "Emacs timer for this timer.")
  (etc nil :documentation "Alist which may be used to store any extra data.")
  (before nil :documentation "Function called before running timer.
Called with one argument, the timer.")
  (after nil :documentation "Function called after timer's cycles have completed.
Called with one argument, the timer."))

(cl-defstruct hammy-interval
  (name "" :type 'string)
  (documentation "" :documentation "Documentation.") ; !
  (length nil :documentation "Number of seconds or function that returns such.
If a function, it is given one argument, the timer it is being
run in.")
  (before nil :documentation "Function called before interval begins.
Called with one argument, the Hammy timer.")
  (after nil :documentation "Function called when interval ends.
Called with one argument, the Hammy timer."))

(define-error 'hammy-hammy-over "Hammy is over!")

;;;; Macros

(defmacro make-hammy (&rest args)
  "Return a HAMMY hammy made with ARGS.
Within ARGS, these functions are available to be called:

  `announce':"
  ;; FIXME: Docstring.
  `(cl-labels ((announce (message)
                         (lambda (hammy)
                           (hammy-announce message hammy)))
               (duration (interval)
                         (timer-duration interval) )
               (interval (&rest args)
                         (apply #'make-hammy-interval args))
               (num-intervals (hammy)
                              (ring-length (hammy-hammy-intervals hammy)))
               (elapsed (hammy)
                        (hammy-hammy-elapsed hammy))
               (cycles (hammy)
                       (hammy-hammy-cycles hammy))
               (climb (from to &key descend then)
                      ;; FIXME: Make arguments optional.
                      (lambda (hammy)
                        (let* ((apex (/ (duration to)
                                        (duration from))))
                          (pcase then
                            ('stop (when (= apex (cycles hammy))
                                     (signal 'hammy-hammy-over hammy))))
                          (if (< (cycles hammy) apex)
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
                                              (cycles (1+ cycles)))
                                            (duration from))
                                         (duration to)))
                              (`t (min (* (pcase (- (cycles hammy) apex)
                                            (0 1)
                                            (cycles (1+ cycles)))
                                          (duration from))
                                       (duration to)))))))))
     (let* ((hammy (make-hammy-hammy ,@args))
            (ring (make-ring (length (hammy-hammy-intervals hammy)))))
       (dolist (interval (hammy-hammy-intervals hammy))
         (ring-insert-at-beginning ring interval))
       (setf (hammy-hammy-intervals hammy) ring)
       hammy)))

;;;; Variables

(defvar hammy-flywheel
  (make-hammy :name "Flywheel"
              :documentation "Get your momentum going!"
              :intervals (list (interval :name "Play"
                                         :length (climb "5 minutes" "15 minutes")
                                         :before (announce "        Play time!")
                                         :after (announce "Play time is over!"))
                               (interval :name "Work"
                                         :length (climb "5 minutes" "45 minutes"
                                                        :descend t :then 'stop)
                                         :before (announce "        Work time!")
                                         :after (announce "Work time is over!"))))
  "A standard Hammy flywheel timer.")

;;;; Commands

(defun hammy-stop (hammy &optional quietly)
  "Stop HAMMY timer.
If QUIETLY, don't say so."
  (interactive
   (list (hammy-complete)))
  (pcase-let (((cl-struct hammy-hammy (timer internal-timer)) hammy))
    (when internal-timer
      (cancel-timer internal-timer)
      (setf (hammy-hammy-timer hammy) nil)
      (unless quietly
        (message "Hammy stopped: %s (%s)"
                 ;; TODO: Logging, totals, etc.
                 (hammy-hammy-name hammy)
                 (hammy-hammy-documentation hammy))))))

(defun hammy-reset (hammy &optional restartp)
  "Reset HAMMY timer.
Sets its elapsed cycles back to 0.  If RESTARTP, also run it
again."
  (interactive (list (hammy-complete) current-prefix-arg))
  (hammy-stop hammy)
  (setf (hammy-hammy-cycles hammy) 0
        (hammy-hammy-elapsed hammy) nil
        (hammy-hammy-current-interval hammy) nil)
  (when restartp
    (hammy-run hammy)))

(defun hammy-toggle (hammy)
  "Toggle HAMMY timer.
If paused, resume it.  If running, pause it."
  (interactive (list (hammy-complete)))
  (if (hammy-hammy-timer hammy)
      (let ((remaining-time (float-time (time-subtract (timer--time hammy) (current-time)))))
        (setf (alist-get 'remaining-time (hammy-hammy-etc hammy)) remaining-time)
        (hammy-stop hammy))
    (hammy-run hammy (alist-get 'remaining-time (hammy-hammy-etc hammy)))
    (setf (alist-get 'remaining-time (hammy-hammy-etc hammy)) nil)))

;;;; Functions

(defun hammy-run (hammy &optional duration)
  "Run HAMMY timer.
If DURATION, set its first interval to last that many seconds."
  (when (and (= 0 (hammy-hammy-cycles hammy))
             (null (hammy-hammy-elapsed hammy))
             (hammy-hammy-before hammy))
    ;; Starting timer: run before function.
    (funcall (hammy-hammy-elapsed hammy) hammy))
  (when (equal (hammy-hammy-current-interval hammy)
               (ring-ref (hammy-hammy-intervals hammy)
                         (1- (ring-length (hammy-hammy-intervals hammy)))))
    ;; Timer has completed a cycle: increment counter.
    (cl-incf (hammy-hammy-cycles hammy)))
  (hammy-stop hammy 'quietly)
  (pcase-let* (((cl-struct hammy-hammy current-interval) hammy)
               (next-interval (if current-interval
                                  (ring-next (hammy-hammy-intervals hammy) current-interval)
                                (ring-ref (hammy-hammy-intervals hammy) 0)))
               (next-duration (or duration
                                  (cl-etypecase (hammy-interval-length next-interval)
                                    (number (hammy-interval-length next-interval))
                                    (function (condition-case _err
                                                  (funcall (hammy-interval-length next-interval) hammy)
                                                (hammy-hammy-over
                                                 (message "Hammy is over!  (%s)" (hammy-hammy-name hammy))
                                                 0)))))))
    (when current-interval
      (when-let ((after (hammy-interval-after current-interval)))
        ;; Announce before updating slots.
        (funcall after hammy))
      (push current-interval (hammy-hammy-elapsed hammy)))    
    (when next-interval
      (setf (hammy-hammy-current-interval hammy) next-interval
            (hammy-hammy-current-duration hammy) next-duration
            (hammy-hammy-timer hammy) (run-at-time next-duration nil #'hammy-run hammy))
      (when (hammy-interval-before next-interval)
        ;; Announce after updating slots.
        (funcall (hammy-interval-before next-interval) hammy))))
  hammy)

(cl-defun hammy-announce (message hammy &key after-interval before-interval)
  "Announce MESSAGE for HAMMY."
  (pcase-let (((cl-struct hammy-hammy current-interval current-duration) hammy))
    (message "%s  (Timer:%s%s%s%s%s%s)"
             message
             (hammy-hammy-name hammy)
             (if after-interval
                 (format "  Interval ended:%s"
                         ;; FIXME: Also add seconds, but doing this for
                         ;; each interval gets confusing since we can't
                         ;; just call the length function again...or can
                         ;; we?
                         (hammy-interval-name after-interval))
               "")
             (if before-interval
                 (format "  Interval ended:%s"
                         ;; FIXME: Also add seconds, but doing this for
                         ;; each interval gets confusing since we can't
                         ;; just call the length function again...or can
                         ;; we?
                         (hammy-interval-name before-interval))
               "")
             (if current-interval
                 (format "  Current interval:%s" (hammy-interval-name current-interval))
               "")
             (if current-duration
                 ;; FIXME: Upscale units.
                 (format "  Current duration:%s seconds" current-duration)
               "")
             (format "  Cycles:%s" (hammy-hammy-cycles hammy)))))

(defun hammy-complete ()
  "Return an active Hammy selected with completion."
  (let ((hammys (hammy--active-timers)))
    (pcase (length hammys)
      (0 (user-error "No active Hammys"))
      (1 (car (timer--args (car hammys))))
      (_ (let* ((selected-timer-name
                 (completing-read "Stop timer: "
                                  (mapcar (lambda (timer)
                                            (hammy-hammy-name (car (timer--args timer))))
                                          hammys)
                                  nil t)))
           (car
            (timer--args
             (cl-find selected-timer-name timer-list
                      :test #'equal
                      :key (lambda (timer)
                             (when (hammy-hammy-p (car (timer--args timer)))
                               (hammy-hammy-name (car (timer--args timer)))))))))))))

(defun hammy--active-timers ()
  "Return any active Emacs timers which are Hammys."
  (cl-remove-if-not (lambda (timer)
                      (equal #'hammy-run (timer--function timer)))
                    timer-list))

;;;; Footer

(provide 'hammy)

;;; hammy.el ends here

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

(require 'cl-lib)

(cl-defstruct hammy-timer
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

(cl-defun hammy-announce (message timer &key after-interval before-interval)
  (pcase-let (((cl-struct hammy-timer current-interval current-duration) timer))
    (message "%s  (Timer:%s%s%s%s%s%s)"
             message
             (hammy-timer-name timer)
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
             (format "  Cycles:%s" (hammy-timer-cycles timer)))))

(define-error 'hammy-timer-over "Hammy is over!")

(defmacro hammy (&rest body)
  `(cl-labels ((announce (message)
                         (lambda (timer)
                           (hammy-announce message timer)))
               (duration (interval)
                         (timer-duration interval) )
               (interval (&rest args)
                         (apply #'make-hammy-interval args))
               (num-intervals (timer)
                              (ring-length (hammy-timer-intervals timer)))
               (elapsed (timer)
                        (hammy-timer-elapsed timer))
               (cycles (timer)
                       (hammy-timer-cycles timer))
               (climb (from to &key descend then)
                      ;; FIXME: Make arguments optional.
                      (lambda (timer)
                        (let* ((apex (/ (duration to)
                                        (duration from))))
                          (pcase then
                            ('stop (when (= apex (cycles timer))
                                     (signal 'hammy-timer-over timer))))
                          (if (< (cycles timer) apex)
                              ;; Spin up!
                              (min (* (pcase (cycles timer)
                                        (0 1)
                                        (cycles (1+ cycles)))
                                      (duration from))
                                   (duration to))
                            ;; Spin down...
                            (pcase-exhaustive descend
                              (`nil (min (* (pcase (cycles timer)
                                              (0 1)
                                              (cycles (1+ cycles)))
                                            (duration from))
                                         (duration to)))
                              (`t (min (* (pcase (- (cycles timer) apex)
                                            (0 1)
                                            (cycles (1+ cycles)))
                                          (duration from))
                                       (duration to)))))))))
     (let* ((timer (make-hammy-timer ,@body))
            (ring (make-ring (length (hammy-timer-intervals timer)))))
       (dolist (interval (hammy-timer-intervals timer))
         (ring-insert-at-beginning ring interval))
       (setf (hammy-timer-intervals timer) ring)
       timer)))

(defvar hammy-flywheel
  (hammy :name "Flywheel"
         :documentation "Get your momentum going!"
         :intervals (list (interval :name "Play"
                                    :length (lambda (timer)
                                              (min (* (pcase (cycles timer)
                                                        (0 1)
                                                        (cycles cycles))
                                                      (duration "5 minutes"))
                                                   (duration "15 minutes")))
                                    :before (announce "Play time!")
                                    :after (announce "Play time is over!"))
                          (interval :name "Work"
                                    :length (lambda (timer)
                                              (if (< (cycles timer) 9)
                                                  ;; Wind up!
                                                  (min (* (pcase (cycles timer)
                                                            (0 1)
                                                            (cycles cycles))
                                                          (duration "5 minutes"))
                                                       (duration "45 minutes"))
                                                ;; Wind down...
                                                (min (* (pcase (- 9 (cycles timer))
                                                          (0 1)
                                                          (cycles cycles))
                                                        (duration "5 minutes"))
                                                     (duration "45 minutes"))))
                                    :before (announce "Work time!")
                                    :after (announce "Work time is over!")))))

(defun hammy-run (timer &optional duration)
  (when (and (= 0 (hammy-timer-cycles timer))
             (null (hammy-timer-elapsed timer))
             (hammy-timer-before timer))
    ;; Starting timer: run before function.
    (funcall (hammy-timer-elapsed timer) timer))
  (when (equal (hammy-timer-current-interval timer)
               (ring-ref (hammy-timer-intervals timer)
                         (1- (ring-length (hammy-timer-intervals timer)))))
    ;; Timer has completed a cycle: increment counter.
    (cl-incf (hammy-timer-cycles timer)))
  (hammy-stop timer 'quietly)
  (pcase-let* (((cl-struct hammy-timer current-interval) timer)
               (next-interval (if current-interval
                                  (ring-next (hammy-timer-intervals timer) current-interval)
                                (ring-ref (hammy-timer-intervals timer) 0)))
               (next-duration (or duration
                                  (cl-etypecase (hammy-interval-length next-interval)
                                    (number (hammy-interval-length next-interval))
                                    (function (condition-case _err
                                                  (funcall (hammy-interval-length next-interval) timer)
                                                (hammy-timer-over
                                                 (message "Hammy is over!  (%s)" (hammy-timer-name timer))
                                                 0)))))))
    (when current-interval
      (when-let ((after (hammy-interval-after current-interval)))
        ;; Announce before updating slots.
        (funcall after timer))
      (push current-interval (hammy-timer-elapsed timer)))    
    (when next-interval
      (setf (hammy-timer-current-interval timer) next-interval
            (hammy-timer-current-duration timer) next-duration
            (hammy-timer-timer timer) (run-at-time next-duration nil #'hammy-run timer))
      (when (hammy-interval-before next-interval)
        ;; Announce after updating slots.
        (funcall (hammy-interval-before next-interval) timer))))
  timer)

(defun hammy--active-timers ()
  (cl-remove-if-not (lambda (timer)
                      (equal #'hammy-run (timer--function timer)))
                    timer-list))

(defun hammy-stop (timer &optional quietly)
  (interactive
   (list (hammy-complete)))
  (pcase-let (((cl-struct hammy-timer (timer internal-timer)) timer))
    (when internal-timer
      (cancel-timer internal-timer)
      (setf (hammy-timer-timer timer) nil)
      (unless quietly
        (message "Hammy stopped: %s (%s)"
                 (hammy-timer-name timer)
                 (hammy-timer-description timer))))))

(defun hammy-reset (timer &optional restartp)
  (interactive (list (hammy-complete) current-prefix-arg))
  (hammy-stop timer)
  (setf (hammy-timer-cycles timer) 0
        (hammy-timer-current-interval timer) nil)
  (when restartp
    (hammy-run timer)))

(defun hammy-toggle (timer)
  (interactive (list (hammy-complete)))
  (if (hammy-timer-timer timer)
      (let ((remaining-time (float-time (time-subtract (timer--time timer) (current-time)))))
        (setf (alist-get 'remaining-time (hammy-timer-etc timer)) remaining-time)
        (hammy-stop timer))
    (hammy-run timer (alist-get 'remaining-time (hammy-timer-etc timer)))
    (setf (alist-get 'remaining-time (hammy-timer-etc timer)) nil)))

(defun hammy-complete ()
  (let ((hammys (hammy--active-timers)))
    (pcase (length hammys)
      (0 (user-error "No active Hammys"))
      (1 (car (timer--args (car hammys))))
      (_ (let* ((selected-timer-name
                 (completing-read "Stop timer: "
                                  (mapcar (lambda (timer)
                                            (hammy-timer-name (car (timer--args timer))))
                                          hammys)
                                  nil t)))
           (car
            (timer--args
             (cl-find selected-timer-name timer-list
                      :test #'equal
                      :key (lambda (timer)
                             (when (hammy-timer-p (car (timer--args timer)))
                               (hammy-timer-name (car (timer--args timer)))))))))))))

(provide 'hammy)
;;; hammy.el ends here

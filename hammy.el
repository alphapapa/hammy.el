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

;;; Code:

(require 'cl-lib)

(cl-defstruct hammy-timer
  (name "" :type 'string) (documentation "" :documentation "Documentation.")
  (elapsed nil :documentation "List of elapsed intervals."
           ;; FIXME: This.
           )
  (cycles 0 :documentation "Number of times the timer has gone through a cycle of all intervals.")
  (intervals nil :documentation "List of defined intervals.")
  (current-interval)
  (last-duration nil :documentation "Length in seconds of last interval.")
  (timer nil :documentation "Emacs timer for this timer.")
  )

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

(defun hammy-announce (timer)
  (message "Timer:%S  Current interval:%S"
           (hammy-timer-name timer)
           (or (ignore-errors
                 (hammy-interval-name (hammy-timer-current-interval timer)))
               "None")))

(defmacro hammy (&rest body)
  `(cl-labels ((duration (interval)
                         (timer-duration interval) )
               (interval (&rest args)
                         (apply #'make-hammy-interval args))
               (elapsed (timer)
                        (hammy-timer-elapsed timer))
               (cycles (timer)
                       (hammy-timer-cycles timer)))
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
                                    :before #'hammy-announce
                                    :after #'hammy-announce)
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
                                    :before #'hammy-announce
                                    :after #'hammy-announce))))

(defun hammy-next (timer)
  (pcase-let* (((cl-struct hammy-timer current-interval) timer)
               (next-interval (if current-interval
                                  (ring-next (hammy-timer-intervals timer) current-interval)
                                (ring-ref (hammy-timer-intervals timer) 0)))
               (next-duration (cl-etypecase (hammy-interval-length next-interval)
                                (number (hammy-interval-length next-interval))
                                (function (funcall (hammy-interval-length next-interval) timer)))))
    (hammy-stop timer)
    (when current-interval
      (push current-interval (hammy-timer-elapsed current-interval))
      (when (hammy-interval-after current-interval)
        (funcall (hammy-interval-after current-interval) timer)))
    (when (equal current-interval (ring-ref (hammy-timer-intervals timer) 0))
      ;; Timer has completed a cycle.
      (cl-incf (hammy-timer-cycles timer)))
    (when next-interval
      (when (hammy-interval-before next-interval)
        (funcall (hammy-interval-before next-interval) timer))
      (setf (hammy-timer-current-interval timer) next-interval
            (hammy-timer-timer timer) (run-at-time next-duration nil #'hammy-next timer))))
  timer)

(defun hammy--timers ()
  (cl-remove-if-not (lambda (timer)
                      (equal #'hammy-next (timer--function timer)))
                    timer-list))

(defun hammy-stop (timer)
  (interactive (list (let* ((selected-timer-name
                             (completing-read "Stop timer: "
                                              (mapcar (lambda (timer)
                                                        (hammy-timer-name (car (timer--args timer))))
                                                      (or (hammy--timers)
                                                          (user-error "No active Hammy timers")))
                                              nil t)))
                       (car
                        (timer--args
                         (cl-find selected-timer-name timer-list
                                  :test #'equal
                                  :key (lambda (timer)
                                         (when (hammy-timer-p (car (timer--args timer)))
                                           (hammy-timer-name (car (timer--args timer)))))))))))
  (pcase-let (((cl-struct hammy-timer (timer internal-timer)) timer))
    (when internal-timer
      (cancel-timer internal-timer)
      (setf (hammy-timer-timer timer) nil)
      (message "Timer stopped: %s (%s)"
               (hammy-timer-name timer)
               (hammy-timer-description timer)))))

(provide 'hammy)
;;; hammy.el ends here

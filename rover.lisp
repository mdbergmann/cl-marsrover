(defpackage :cl-rover.rover
  (:use :cl :cl-gserver :cl-gserver.actor)
  (:nicknames :rover)
  (:export #:rover)
  )

(in-package :cl-rover.rover)

(defstruct rover-state
  (x-position 1 :type integer)
  (y-position 1 :type integer)
  (direction :north :type symbol))

(defclass rover (actor)
  ((reporter :initarg :reporter
             :initform nil)))

(defmethod initialize-instance :after ((self rover) &key)
  (with-slots (reporter cl-gserver::state) self
    (when (null cl-gserver::state)
      (setf cl-gserver::state (make-rover-state)))

    (report-state self cl-gserver::state)))

(defmethod receive ((self rover) message current-state)
  (let ((new-state (process-commands message (copy-structure current-state))))
    (report-state self new-state)
    (cons new-state new-state)))

(defun process-commands (commands rover-state)
  (reduce #'(lambda (rover-state command)
              (cond
                ((string= "f" command) (move-forward rover-state)))
              ) commands :initial-value rover-state))

(defun move-forward (rover-state)
  (incf (rover-state-y-position rover-state))
  rover-state)

(defun report-state (rover state)
  (with-slots (reporter) rover
    (when reporter
      (send reporter (create-reporting-state state)))))

(defun create-reporting-state (state)
  (list
   (cons
    :position
    (cons
     (rover-state-x-position state)
     (rover-state-y-position state)))
   (cons
    :direction
    (rover-state-direction state))))

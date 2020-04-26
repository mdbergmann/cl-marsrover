(defpackage :cl-rover.rover
  (:use :cl :cl-gserver :cl-gserver.actor)
  (:nicknames :rover)
  (:export #:rover
           #:rover-state
           #:make-rover-state))

(in-package :cl-rover.rover)

(defstruct rover-state
  (x-position 1 :type integer)
  (y-position 1 :type integer)
  (direction :north :type symbol))

(defclass rover (actor) ())

(defmethod receive ((self actor) message current-state)
  (cond
    ((eq message :get-position)
     (cons
      (cons
       (rover-state-x-position current-state)
       (rover-state-y-position current-state))
      current-state))
    ((eq message :get-direction)
     (cons
      (rover-state-direction current-state)
      current-state))
    ((eq (car message) :execute)
     (cons
      nil
      (apply-command-for-new-state (cdr message) (copy-structure current-state)))))
  )

(defun apply-command-for-new-state (commands rover-state)
  (reduce #'process-command commands :initial-value rover-state))

(defun process-command (rover-state command)
  (cond
    ((string= "f" command) (detect-obstacle (fix-grid-wrapping (movef-forward rover-state))))
    ((string= "b" command) (detect-obetacle (fix-grid-wrapping (movef-backward rover-state))))
    ((string= "l" command) (turnf-left rover-state))
    ((string= "r" command) (turnf-right rover-state))
    (t rover-state))
  )

(defun movef-forward (rover-state)
  (case (rover-state-direction rover-state)
    (:east (incf (rover-state-x-position rover-state)))
    (:south (decf (rover-state-y-position rover-state)))
    (:west (decf (rover-state-x-position rover-state)))
    (:north (incf (rover-state-y-position rover-state))))
  rover-state)

(defun movef-backward (rover-state)
  (case (rover-state-direction rover-state)
    (:east (decf (rover-state-x-position rover-state)))
    (:south (incf (rover-state-y-position rover-state)))
    (:west (incf (rover-state-x-position rover-state)))
    (:north (decf (rover-state-y-position rover-state))))
  rover-state)

(defun turnf-left (rover-state)
  (case (rover-state-direction rover-state)
    (:north (setf (rover-state-direction rover-state) :west))
    (:west (setf (rover-state-direction rover-state) :south))
    (:south (setf (rover-state-direction rover-state) :east))
    (:east (setf (rover-state-direction rover-state) :north)))  
  rover-state)

(defun turnf-right (rover-state)
  (let ((direction (rover-state-direction rover-state)))
    (case direction
      (:north (setf (rover-state-direction rover-state) :east))
      (:east (setf (rover-state-direction rover-state) :south))
      (:south (setf (rover-state-direction rover-state) :west))
      (:west (setf (rover-state-direction rover-state) :north))))
  rover-state)

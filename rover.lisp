(defpackage :cl-rover.rover
  (:use :cl :cl-gserver.actor)
  (:nicknames :rover)
  (:export #:rover
           #:rover-state
           #:make-rover-state))

(in-package :cl-rover.rover)

(defstruct rover-state
  (x-position 1 :type integer)
  (y-position 1 :type integer)
  (facing :north :type symbol))

(defclass rover (actor) ())

(defmethod receive ((self rover) message current-state)
  (cond
    ((eq message :get-position)
     (cons
      (cons (rover-state-x-position current-state) (rover-state-y-position current-state))
      current-state))
    ((eq message :get-facing)
     (cons
      (rover-state-facing current-state)
      current-state))
    ((eq (car message) :execute)
     (let ((new-state
             (apply-commands-for-new-state (cdr message) (copy-structure current-state))))
       (cons new-state new-state)))))

(defun apply-commands-for-new-state (commands rover-state)
  (reduce #'process-command commands :initial-value rover-state))

(defun process-command (state cmd)
  (format t "cmd: ~a~%" cmd)
  (format t "state: ~a~%" state)
  (let ((new-state (copy-structure state)))
    (cond
      ((string= "f" cmd) (movef-forward new-state))
      ((string= "b" cmd) (movef-backward new-state))
      (t new-state))))

(defun movef-forward (rover-state)  
  (incf (rover-state-y-position rover-state))
  rover-state)
(defun movef-backward (rover-state)  
  (decf (rover-state-y-position rover-state))
  rover-state)

(defpackage :cl-rover.rover-test
  (:use :cl :fiveam :cl-rover.rover :cl-gserver :cl-gserver.actor)
  (:export #:run!
           #:all-tests
           #:nil))
(in-package :cl-rover.rover-test)

(def-suite rover-tests
  :description "Tests for rover")

(in-suite rover-tests)

(test create-rover
  "Test to create a rover."
  (is (not (null (make-instance 'rover)))))

(test initialize-rover
  "Tests to initialize rover and state."

  (let* ((reported-state nil)
         (reporter (make-actor "reporter"
                               :state reported-state
                               :receive-fun
                               (lambda (self msg state)
                                 (setf reported-state msg)
                                 (cons msg msg))))
         (rover (make-instance 'rover :reporter reporter)))
    (is (eq t (assert-state
               (lambda ()
                 (equal (cons 1 1) (get-position reported-state)))
               3)))
    (is (eq t (assert-state
               (lambda ()
                 (eq :north (get-direction reported-state)))
               3)))))

(test move-forward
  "Test for moving forward"

  (let* ((reported-state nil)
         (reporter (make-actor "reporter"
                               :state reported-state
                               :receive-fun
                               (lambda (self msg state)
                                 (setf reported-state msg)
                                 (cons msg msg))))
         (rover (make-instance 'rover :reporter reporter)))
    (send rover '("f"))
    (is (eq t (assert-state
               (lambda ()
                 (equal (cons 1 2) (get-position reported-state)))
               3)))    
  ))

(defun get-position (alist)
  (cdr (assoc :position alist)))

(defun get-direction (alist)
  (cdr (assoc :direction alist)))

(defun assert-state (fun max-time)
  (do ((wait-time 0.05 (+ wait-time 0.5))
       (fun-result nil (funcall fun)))
      ((eq fun-result t) (return t))
    (if (> wait-time max-time) (return)
        (sleep 0.05))))

(run! 'create-rover)
(run! 'initialize-rover)
(run! 'move-forward)

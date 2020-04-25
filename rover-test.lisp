(defpackage :cl-rover.rover-test
  (:use :cl :fiveam :cl-rover.rover)
  (:export #:run!
           #:all-tests
           #:nil))
(in-package :cl-rover.rover-test)

(def-suite rover-tests
  :description "Rover tests")

(in-suite rover-tests)

(test initialize-rover
  "Test for initializing the rover"

  (let* ((rover-state (make-initial-rover-state))
         (rover (make-instance 'rover :state rover-state)))
    (is (not (null rover)))
    (is (equal (cons 1 1) (act:ask rover :get-position)))
    (is (eq :north (act:ask rover :get-facing)))))

(test send-commands-fb
  "Tests for forward/backward commands"

  (let* ((rover-state (make-initial-rover-state))
         (rover (make-instance 'rover :state rover-state)))

    ;; forward
    (is (eq t (act:send rover (cons :execute '("f")))))
    (is (eq t (assert-cond
               (lambda ()
                 (equal (cons 1 2) (act:ask rover :get-position))) 1)))

    (is (eq t (act:send rover (cons :execute '("f" "f" "f")))))
    (is (eq t (assert-cond
               (lambda ()
                 (equal (cons 1 5) (act:ask rover :get-position))) 1)))

    ;; backward
    (is (eq t (act:send rover (cons :execute '("b")))))    
    (is (eq t (assert-cond
               (lambda ()
                 (equal (cons 1 4) (act:ask rover :get-position))) 1)))

    (is (eq t (act:send rover (cons :execute '("b" "b" "b")))))    
    (is (eq t (assert-cond
               (lambda ()
                 (equal (cons 1 1) (act:ask rover :get-position))) 1)))
    
    ))

(defun assert-cond (assert-fun max-time)
  (do ((wait-time 0.02 (+ wait-time 0.02))
       (fun-result nil (funcall assert-fun)))
      ((eq fun-result t) (return t))
    (if (> wait-time max-time) (return)
        (sleep 0.02))))

(defun make-initial-rover-state ()
  (rover:make-rover-state
   :x-position 1
   :y-position 1
   :facing :north))

(run! 'initialize-rover)
(run! 'send-commands-fb)

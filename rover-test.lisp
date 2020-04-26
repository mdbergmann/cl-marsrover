(defpackage :cl-rover.rover-test
  (:use :cl :fiveam :cl-rover.rover)
  (:export #:run!
           #:all-tests
           #:nil))
(in-package :cl-rover.rover-test)

(def-suite rover-tests
  :description "Tests for the MarsRover")

(in-suite rover-tests)

(test create-rover
  "Test to create a rover."
  (is (not (null (make-instance 'rover)))))

(test initialize-rover
  "Tests the initial state of the rover."

  (let ((rover (make-instance 'rover :state (make-initial-rover-state))))
    (is (equal (cons 1 1) (act:ask rover :get-position)))
    (is (eq :north (act:ask rover :get-direction)))
  ))

(test move-rover-fb
  "Tests for moving the rover forward/backward."

  (let ((rover (make-instance 'rover :state (make-initial-rover-state))))
    ;; forward
    (is (eq t (act:send rover (cons :execute '("f")))))
    (is (eq t (assert-state
               (lambda ()
                 (equal (cons 1 2) (act:ask rover :get-position))) 1)))

    (is (eq t (act:send rover (cons :execute '("f" "f" "f")))))
    (is (eq t (assert-state
               (lambda ()
                 (equal (cons 1 5) (act:ask rover :get-position))) 1)))

    ;; backward
    (is (eq t (act:send rover (cons :execute '("b")))))
    (is (eq t (assert-state
               (lambda ()
                 (equal (cons 1 4) (act:ask rover :get-position))) 1)))

    (is (eq t (act:send rover (cons :execute '("b" "b" "b")))))
    (is (eq t (assert-state
               (lambda ()
                 (equal (cons 1 1) (act:ask rover :get-position))) 1)))
    )
  )

(test turn-rover-lb
  "Tests for turning the rover left/right."

  (let ((rover (make-instance 'rover :state (make-initial-rover-state))))
    ;; left
    (is (eq t (act:send rover (cons :execute '("l")))))
    (is (eq t (assert-state
               (lambda ()
                 (eq :west (act:ask rover :get-direction))) 1)))

    (is (eq t (act:send rover (cons :execute '("l" "l" "l")))))
    (is (eq t (assert-state
               (lambda ()
                 (eq :north (act:ask rover :get-direction))) 1)))

    ;; right
    (is (eq t (act:send rover (cons :execute '("r")))))
    (is (eq t (assert-state
               (lambda ()
                 (eq :east (act:ask rover :get-direction))) 1)))
    
    (is (eq t (act:send rover (cons :execute '("r" "r" "r")))))
    (is (eq t (assert-state
              (lambda ()
                (eq :north (act:ask rover :get-direction))) 1)))
    )
  )

(test move-depends-on-direction
  "Tests for moving with taking direction into account."

  (let ((rover (make-instance 'rover :state (make-initial-rover-state))))
    ;; direction east
    (act:send rover (cons :execute '("r")))
    (is (eq t (assert-state
               (lambda ()
                 (eq :east (act:ask rover :get-direction))) 1)))
    (is (eq t (act:send rover (cons :execute '("f")))))
    (is (eq t (assert-state
               (lambda ()
                 (equal (cons 2 1) (act:ask rover :get-position))) 1)))
    (is (eq t (act:send rover (cons :execute '("b")))))
    (is (eq t (assert-state
               (lambda ()
                 (equal (cons 1 1) (act:ask rover :get-position))) 1)))
    
    ;; direction south
    (act:send rover (cons :execute '("r")))
    (is (eq t (assert-state
               (lambda ()
                 (eq :south (act:ask rover :get-direction))) 1)))
    (is (eq t (act:send rover (cons :execute '("f")))))
    (is (eq t (assert-state
               (lambda ()
                 (equal (cons 1 0) (act:ask rover :get-position))) 1)))
    (is (eq t (act:send rover (cons :execute '("b")))))
    (is (eq t (assert-state
               (lambda ()
                 (equal (cons 1 1) (act:ask rover :get-position))) 1)))

    ;; direction west
    (act:send rover (cons :execute '("r")))
    (is (eq t (assert-state
               (lambda ()
                 (eq :west (act:ask rover :get-direction))) 1)))
    (is (eq t (act:send rover (cons :execute '("f")))))
    (is (eq t (assert-state
               (lambda ()
                 (equal (cons 0 1) (act:ask rover :get-position))) 1)))
    (is (eq t (act:send rover (cons :execute '("b")))))
    (is (eq t (assert-state
               (lambda ()
                 (equal (cons 1 1) (act:ask rover :get-position))) 1)))

    ;; direction north
    (act:send rover (cons :execute '("r")))
    (is (eq t (assert-state
               (lambda ()
                 (eq :north (act:ask rover :get-direction))) 1)))
    (is (eq t (act:send rover (cons :execute '("f")))))
    (is (eq t (assert-state
               (lambda ()
                 (equal (cons 1 2) (act:ask rover :get-position))) 1)))
    (is (eq t (act:send rover (cons :execute '("b")))))
    (is (eq t (assert-state
               (lambda ()
                 (equal (cons 1 1) (act:ask rover :get-position))) 1)))
    )
  )


(defun assert-state (fun max-time)
  (do ((wait-time 0.05 (+ wait-time 0.5))
       (fun-result nil (funcall fun)))
      ((eq fun-result t) (return t))
    (if (> wait-time max-time) (return)
        (sleep 0.05))))

(defun make-initial-rover-state ()
  (make-rover-state :x-position 1 :y-position 1))

(run! 'create-rover)
(run! 'initialize-rover)
(run! 'move-rover-fb)
(run! 'turn-rover-lb)
(run! 'move-depends-on-direction)

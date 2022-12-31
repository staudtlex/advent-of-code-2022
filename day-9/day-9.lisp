;;;; -*- mode: Lisp; indent-tabs-mode: nil; -*-
;;;; Advent of Code 2022
;;;;
;;;; --- Day 9 ---
(defpackage :aoc-day-9
  (:use :common-lisp))

(in-package :aoc-day-9)

;;; --- Part 1 ---
;;; load and prepare data
(require 'asdf)
(setq data (mapcar #'(lambda (line)
                       (list (subseq line 0 1)
                             (parse-integer (subseq line 2))))
                   (uiop:read-file-lines "input.txt")))


(defun move-head (position direction)
  (let ((x (nth 0 position))
        (y (nth 1 position)))
    (cond ((equal direction "R")
           (list (1+ x) y))
          ((equal direction "L")
           (list (1- x) y))
          ((equal direction "U")
           (list x (1+ y)))
          ((equal direction "D")
           (list x (1- y))))))


(defun point- (pos-1 pos-2)
  (let ((x1 (first pos-1))
        (x2 (first pos-2))
        (y1 (second pos-1))
        (y2 (second pos-2)))
    (list (- x1 x2) (- y1 y2))))


(defun point+ (pos-1 pos-2)
  (let ((x1 (first pos-1))
        (x2 (first pos-2))
        (y1 (second pos-1))
        (y2 (second pos-2)))
    (list (+ x1 x2) (+ y1 y2))))


(defun move-tail (pos-tail pos-head-new pos-head-old)
  (labels ((euclidean-distance (pos-1 pos-2)
             (let ((x0 (first pos-1))
                   (y0 (second pos-1))
                   (x1 (first pos-2))
                   (y1 (second pos-2)))
               (sqrt (+ (expt (- x1 x0) 2)
                        (expt (- y1 y0) 2))))))
    (if (> (euclidean-distance pos-head-new pos-tail) (sqrt 2))
        pos-head-old
        pos-tail)))


(defun move-tail-and-store-visited (pos-tail pos-head-new pos-head-old)
  (let ((pos-tail-new (move-tail pos-tail pos-head-new pos-head-old)))
    (pushnew pos-tail-new *visited-positions* :test #'equal)
    pos-tail-new))


(defun move-rope (pos-head pos-tail motion start)
  (let* ((direction (first motion))
         (times (second motion))
         (pos-head-new (move-head pos-head direction))
         (new-pos (list pos-head-new
                        (move-tail-and-store-visited pos-tail
                                                     pos-head-new
                                                     pos-head))))
    (if (equal start times)
        new-pos
        (move-rope (first new-pos) (second new-pos) motion (1+ start)))))


(defun move (pos-head pos-tail motions)
  (let* ((motion (car motions))
         (new-pos (if motion
                      (move-rope pos-head pos-tail motion 1)
                      nil)))
    (if (not motion)
        (list pos-head pos-tail)
        (move (first new-pos) (second new-pos) (cdr motions)))))


(defvar *visited-positions* nil)


;;; Simulate your complete hypothetical series of motions.
;;; How many positions does the tail of the rope visit at least once?
(let ((*visited-positions* nil))
  (move '(1 1) '(1 1) data)
  (length *visited-positions*))


;;; --- Part 2 ---
(defun move-tail-2 (pos-tail pos-head)
  (let* ((diff (point- pos-head pos-tail))
         (x (first diff))
         (y (second diff)))
    (if (or (>= (abs x) 2) (>= (abs y) 2))
        (cond ((and (= x 0)
                    (> y 0))
               (point+ pos-tail '(0 1)))
              ((and (= x 0)
                    (< y 0))
               (point+ pos-tail '(0 -1)))
              ((and (> x 0)
                    (= y 0))
               (point+ pos-tail '(1 0)))
              ((and (< x 0)
                    (= y 0))
               (point+ pos-tail '(-1 0)))
              ((and (> x 0)
                    (> y 0))
               (point+ pos-tail '(1 1)))
              ((and (> x 0)
                    (< y 0))
               (point+ pos-tail '(1 -1)))
              ((and (< x 0)
                    (< y 0))
               (point+ pos-tail '(-1 -1)))
              ((and (< x 0)
                    (> y 0))
               (point+ pos-tail '(-1 1))))                 
        pos-tail)))


(defun move-tails (positions head acc)
  (let* ((knots (cdr positions))
         (tail (car knots))
         (head-new (if tail
                       (move-tail-2 tail head)))
         (acc-new (if tail
                      (append acc (list head-new))
                      acc)))
    (if (not head-new)
        acc-new
        (move-tails knots head-new acc-new))))


(defvar *visited-positions-2* nil)


(defun move-head-and-tails-by-one (positions direction)
  (let* ((pos-head-old (car positions))
         (pos-head-new (move-head pos-head-old direction))
         (new-positions (if pos-head-new
                            (move-tails positions
                                          pos-head-new
                                          (list pos-head-new)))))
    (pushnew (car (last new-positions)) *visited-positions-2* :test #'equal)
    new-positions))


(defun move-head-and-tails (positions motion start)
  (let* ((direction (first motion))
         (times (second motion))
         (new-positions (move-head-and-tails-by-one positions direction)))
    (if (equal start times)
        new-positions
        (move-head-and-tails new-positions motion (1+ start)))))


(defun move-2 (positions motions)
  (let* ((motion (car motions))
         (new-positions (if motion
                            (move-head-and-tails positions motion 1)
                            nil)))
    (if (not motion)
        positions
        (move-2 new-positions (cdr motions)))))


;;; Simulate your complete series of motions on a larger rope with ten knots.
;;; How many positions does the tail of the rope visit at least once?
(let ((rope (list '(1 1) '(1 1)
                  '(1 1) '(1 1)
                  '(1 1) '(1 1)
                  '(1 1) '(1 1)
                  '(1 1) '(1 1)))
      (*visited-positions-2* nil))
  (move-2 rope data)
  (length *visited-positions-2*))


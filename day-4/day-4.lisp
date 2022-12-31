;;;; -*- mode: Lisp; indent-tabs-mode: nil; -*-
;;;; Advent of Code 2022
;;;;
;;;; --- Day 4: Camp Cleanup ---
(defpackage :aoc-day-4
  (:use :common-lisp))

(in-package :aoc-day-4)

;;; --- Part 1 ---
;;; load and prepare data
(require 'asdf)
(setf input-data (uiop:read-file-lines "input.txt"))


;;; convert comma- and hyphen-separated data into list of pair of lists
;;; of integers
(defun parse-pair (pair)
  ;; preprocess comma- and hyphen-separated data
  (let ((pair-data (mapcar #'(lambda (range-string)
                               (uiop:split-string range-string
                                                  :max nil
                                                  :separator "-"))
                           (uiop:split-string pair
                                              :max nil
                                              :separator ","))))
    ;; parse strings, creating list of pair of lists
    (mapcar #'(lambda (range-info)
                (mapcar #'parse-integer range-info))
            pair-data)))


;;; create range with loop
(defun range-inclusive (min-max)
  (let ((start (first min-max))
        (end (second min-max)))
    (loop :for i :from start :below (1+ end) collect i)))


;;; check if one assignment fully contains the other
(defun fully-contains? (pair)
  (let* ((ranges (mapcar #'range-inclusive pair))
         (common (reduce #'intersection ranges))
         (len (length common)))
    (if (or (equal len (length (first ranges)))
            (equal len (length (second ranges))))
        pair
        nil)))


;;; In how many assignment pairs does one range fully contain the other?
(count-if #'(lambda (x) x) (mapcar #'fully-contains?
                                   (mapcar #'parse-pair input-data)))


;;; --- Part 2 ---
;;; In how many assignment pairs do the ranges overlap?
(count-if #'(lambda (x) x) (mapcar #'(lambda (pair)
                                       (reduce #'intersection
                                               (mapcar #'range-inclusive pair)))
                                   (mapcar #'parse-pair input-data)))

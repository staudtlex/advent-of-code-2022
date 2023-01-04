;;;; -*- mode: Lisp; indent-tabs-mode: nil; -*-
;;;; Advent of Code 2022
;;;;
;;;; --- Day 1: Calorie Counting ---
(defpackage :aoc-day-1
  (:use :common-lisp))

(in-package :aoc-day-1)

;;; --- Part 1 ---
;;; helper function for splitting input data at ""
(defun split (data)
  ;; NOTE: There may very well be built-in/more efficient ways to split a list
  ;; at "" in Common Lisp. I wrote this function for educational purposes, to
  ;; practice how to write recursive functions.
  ;;
  ;; SPLIT-0 takes a list `data' and splits it at the first occurrence of "".
  ;; Returns a list with one list that contains a list of `data''s elements up
  ;; to the first "", and another list with the remaining elements of `data'
  (defun split-0 (data lst)
    (let ((pos (position "" data :test #'equal)))
      (if pos
          (list (append lst (list (subseq data 0 pos)))
                (nthcdr (1+ pos) data))
          (list (append lst (list data))))))
  ;; SPLIT-1 recursively splits `data' at any occurrence of "".
  (defun split-1 (data lst)
    (let* ((init (split-0 data lst))
           (new-lst (car init))
           (new-data (cadr init)))
      (if (not new-data)
          (append (list (car lst)) new-lst)
          (split-1 new-data new-lst))))
  ;; Apply SPLIT-1 to `data'
  (split-1 data nil))


;;; load and prepare data
(require 'asdf)
(setf data (split (uiop:read-file-lines "input.txt")))


;;; prepare data
(setf snack-data (mapcar #'(lambda (d) (mapcar #'parse-integer d)) data))


;;; Find the Elf carrying the most Calories.
;;; How many total Calories is that Elf carrying?
(setf calories (mapcar #'(lambda (elf) (reduce #'+ elf)) snack-data))
(apply #'max calories)


;;; --- Part 2 ---
;;; Find the top three Elves carrying the most Calories.
;;; How many Calories are those Elves carrying in total?
(reduce #'+ (subseq (stable-sort (copy-seq calories) #'>) 0 3))

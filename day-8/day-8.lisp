;;;; -*- mode: Lisp; indent-tabs-mode: nil; -*-
;;;; Advent of Code 2022
;;;;
;;;; --- Day 8: Treetop Tree House ---
(defpackage :aoc-day-8
  (:use :common-lisp))

(in-package :aoc-day-8)

;;; --- Part 1 ---
;;; load and prepare data
(require 'asdf)
(setf input-data (uiop:read-file-lines "input.txt"))
(setf data (mapcar #'(lambda (line)
                       (mapcar #'digit-char-p (coerce line 'list)))
                   input-data))


;;; defin helper functions
(defun get-col-0 (data col column-data)
  (let* ((first-row (car data))
         (remaining-rows (cdr data))
         (col-data (append column-data (list (nth col first-row)))))
    (if (not remaining-rows)
        col-data
        (get-col-0 remaining-rows col col-data))))


(defun get-col (data col)
  (get-col-0 data col nil))


(defun get-row (data row)
  (nth row data))


(defun get-tree (data row col)
  (nth col (nth row data)))


(defun is-visible? (data row col)
  (let* ((tree (nth col (nth row data)))
         (row-data (get-row data row))
         (col-data (get-col data col))
         (right (subseq row-data (1+ col)))
         (left (reverse (subseq row-data 0 col)))
         (down (subseq col-data (1+ row)))
         (up (reverse (subseq col-data 0 row))))
    (or
     (every #'(lambda (x) (> tree x)) right)
     (every #'(lambda (x) (> tree x)) left)
     (every #'(lambda (x) (> tree x)) down)
     (every #'(lambda (x) (> tree x)) up))))


;;; Consider your map; how many trees are visible from outside the grid?
(let* ((rows (length data))
       (cols (length (nth 0 data)))
       (edge-trees (* 2 (+ rows (- cols 2))))
       ;; loop ignores trees on the edge.
       (visible-trees (loop :for row :from 1 :below (1- rows)
                            append (loop :for col :from 1 :below (1- cols)
                                         collect (is-visible? data row col)))))
  (print edge-trees)
  (print (reduce #'+ (mapcar #'(lambda (e) (if e 1 0)) visible-trees)))
  (+ edge-trees
     (reduce #'+ (mapcar #'(lambda (e) (if e 1 0)) visible-trees))))


;;; --- Part 2 ---
;;; define helper function
(defun compute-viewing-distance (data row col)
  (let* ((tree (nth col (nth row data)))
         (row-data (get-row data row))
         (col-data (get-col data col))
         (right (subseq row-data (1+ col)))
         (left (reverse (subseq row-data 0 col)))
         (down (subseq col-data (1+ row)))
         (up (reverse (subseq col-data 0 row)))
         (views (list up right down left)))
    (labels ((compute-view-distance (tree view)
               (let ((test-distance (position tree view :test #'<=)))
                 (cond ((not view) 0)             ; trees on the edge
                       ((<= tree (first view)) 1) ; adjacent tree higher or as high as tree
                       ((> tree (first view))     ; adjacent tree smaller than tree
                        (if test-distance         ; any tree in direction as high as tree?
                            (1+ test-distance)    ; if yes: distance up to equally high tree
                            (length view)))))))   ; if no:  all trees until (including) edge
      (reduce #'* (mapcar #'(lambda (view) (compute-view-distance tree view))
                          views)))))


;;; Consider each tree on your map. What is the highest scenic score possible
;;; for any tree?
(let* ((rows (length data))
       (cols (length (nth 0 data)))
       (scores (loop :for row :from 1 :below (1- rows)
                     append (loop :for col :from 1 :below (1- cols)
                                  collect (compute-viewing-distance data row col)))))
  (reduce #'max scores))


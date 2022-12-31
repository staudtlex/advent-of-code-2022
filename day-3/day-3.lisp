;;;; -*- mode: Lisp; indent-tabs-mode: nil; -*-
;;;; Advent of Code 2022
;;;;
;;;; --- Day 3: Rucksack Reorganization ---
(defpackage :aoc-day-3
  (:use :common-lisp))

(in-package :aoc-day-3)

;;; load and prepare data
(require 'asdf)
(setq rucksack-data (uiop:read-file-lines "input.txt"))

;;; --- Part 1 ---
;;; Find the item type that appears in both compartments of each rucksack.
;;; What is the sum of the priorities of those item types?
(let ((data (mapcar #'(lambda (s)
                        (let ((l (/ (length s) 2)))
                          (list (subseq s 0 l)
                                (subseq s l))))
                    rucksack-data)))
  (reduce #'+ (mapcar #'(lambda (s)
                          (let ((common (car (intersection
                                              (map 'list #'char-code (first s))
                                              (map 'list #'char-code (second s))))))
                            (cond ((upper-case-p (code-char common))
                                   ;; deal with ascii upper case chars having
                                   ;; a smaller value than lower case chars.
                                   (+ (mod common (1- (char-code #\A))) 26))
                                  ((lower-case-p (code-char common))
                                   (mod common (1- (char-code #\a)))))))
                      data)))

;;; --- Part 2 ---
;;; Find the item type that corresponds to the badges of each three-Elf group.
;;; What is the sum of the priorities of those item types?

;;; helper function to create groups
(defun get-three (d)
  (labels ((get-three-0 (x)
             (let ((init (list (append (car x) (list (subseq (cadr x) 0 3)))
                               (subseq (cadr x) 3))))
               (if (not (nth 1 init))
                   init
                   (get-three-0 init)))))
    (car (get-three-0 (list nil d)))))


(let* ((data (get-three rucksack-data))
       (item-priorities (mapcar #'(lambda (rucksack-group)
                                    ;; get item that appears in all three rucksacks
                                    (let ((common (car (reduce #'intersection 
                                                               (mapcar #'(lambda (item)
                                                                           (remove-duplicates
                                                                            (map 'list #'char-code item)))
                                                                       rucksack-group)))))
                                      (cond ((upper-case-p (code-char common))
                                             (+ (mod common (1- (char-code #\A))) 26))
                                            ((lower-case-p (code-char common))
                                             (mod common (1- (char-code #\a)))))))
                                data)))
  (reduce #'+ item-priorities))

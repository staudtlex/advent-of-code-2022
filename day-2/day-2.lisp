;;;; -*- mode: Lisp; indent-tabs-mode: nil; -*-
;;;; Advent of Code 2022
;;;;
;;;; --- Day 2: Rock Paper Scissors ---
(defpackage :aoc-day-2
  (:use :common-lisp))

(in-package :aoc-day-2)

;;; --- Part 1 ---
;;; What would your total score be if everything goes exactly according to your
;;; strategy guide?
(require 'asdf)
(setq strategy-guide (mapcar #'(lambda (e)
                                 (list (subseq e 0 1) (subseq e 2 3)))
                             (uiop:read-file-lines "input.txt")))

(defun play (lst)
  (let ((s-1 (first lst))
        (s-2 (second lst)))
    (cond ((equal lst '("A" "X")) 3) ; rock  vs. rock
          ((equal lst '("A" "Y")) 6) ; rock  vs. paper
          ((equal lst '("A" "Z")) 0) ; rock  vs. scissors
          ((equal lst '("B" "X")) 0) ; paper vs. rock
          ((equal lst '("B" "Y")) 3) ; paper vs. paper
          ((equal lst '("B" "Z")) 6) ; paper vs. scissors
          ((equal lst '("C" "X")) 6) ; scissors vs. rock
          ((equal lst '("C" "Y")) 0) ; scissors vs. paper
          ((equal lst '("C" "Z")) 3) ; scissors vs. scissors
          (t nil))))

(defun compute-score (outcome shape)
  (let ((values (pairlis '("X" "Y" "Z") '(1 2 3))))
    (+ outcome (cdr (assoc shape values :test #'equal)))))

(let ((scores (mapcar #'(lambda (s)
                         (compute-score (play s) (nth 1 s)))
                     strategy-guide)))
  (reduce #'+ scores))

;;; --- Part 2 ---
;;; Following the Elf's instructions for the second column, what would your
;;; total score be if everything goes exactly according to your strategy guide?

(defun make-draw (shape) shape)

(defun make-lose (shape)
  (let ((table (pairlis '("A" "B" "C") '("C" "A" "B"))))
    (cdr (assoc shape table :test #'equal))))

(defun make-win (shape)
  (let ((table (pairlis '("A" "B" "C") '("B" "C" "A"))))
    (cdr (assoc shape table :test #'equal))))

(defun play-2 (lst)
  (let ((shape (first lst))
        (outcome (second lst))
        (table (pairlis '("X" "Y" "Z")
                        (list #'make-lose #'make-draw #'make-win))))
    (funcall (cdr (assoc outcome table :test #'equal)) shape)))

(defun compute-score-2 (outcome shape)
  (let ((shape-values (pairlis '("A" "B" "C") '(1 2 3)))
        (outcome-values (pairlis '("X" "Y" "Z") '(0 3 6))))
    (+ (cdr (assoc outcome outcome-values :test #'equal))
       (cdr (assoc shape shape-values :test #'equal)))))

(let ((scores-2 (mapcar #'(lambda (s)
                           (compute-score-2 (nth 1 s) (play-2 s)))
                       strategy-guide)))
  (reduce #'+ scores-2))


;;; --- A more elegant solution, analogous to 
;;; https://twitter.com/antoine_fabri/status/1598686126079938561

;;; Read data and transform letters into numbers
(setq strategy-guide-2
      (let ((letters-int-table (pairlis '("A" "B" "C" "X" "Y" "Z")
                                        '(1 2 3 1 2 3))))
        (mapcar #'(lambda (e)
                    (list (cdr (assoc (subseq e 0 1) letters-int-table
                                      :test #'equal))
                          (cdr (assoc (subseq e 2 3) letters-int-table
                                      :test #'equal))))
                (uiop:read-file-lines "input.txt"))))

;;; --- Part 1 ---
;;; What would your total score be if everything goes exactly according to your
;;; strategy guide?
(reduce #'+ (mapcar #'(lambda (s)
                        (+ (second s)
                           (* (mod (1+ (reduce #'- (reverse s))) 3)
                              3)))
                    strategy-guide-2))

;;; --- Part 2 ---
;;; Following the Elf's instructions for the second column, what would your
;;; total score be if everything goes exactly according to your strategy guide?
(reduce #'+ (mapcar #'(lambda (s)
                        (+ (mod (reduce #'+ s) 3)
                           1
                           (* 3 (1- (second s)))))
                    strategy-guide-2))

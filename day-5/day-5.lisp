;;;; -*- mode: Lisp; indent-tabs-mode: nil; -*-
;;;; Advent of Code 2022
;;;;
;;;; --- Day 5: Supply Stacks ---
(defpackage :aoc-day-5
  (:use :common-lisp))

(in-package :aoc-day-5)

;;; --- Part 1 ---
;;; load and prepare data
(require 'asdf)
(setf input-data (let* ((input-data (uiop:read-file-lines "input.txt"))
                        (pos (position "" input-data :test #'equal)))
                   (append (list (subseq input-data 0 (1- pos)))  ; stacks
                           (list (subseq input-data (1+ pos)))))) ; rearrangement procedures


(setf stacks
      (let ((columns '(2 6 10 14 18 22 26 30 34)))
        (mapcar #'(lambda (column)
                    (remove " " (mapcar #'(lambda (line)
                                            (subseq line (1- column) column))
                                        (car input-data))
                            :test #'equal))
                columns)))


(setf instructions
      (mapcar #'(lambda (line)
                  (let ((line-data (uiop:split-string line :max nil :separator " ")))
                    (remove-if #'not (mapcar #'(lambda (e)
                                                 (parse-integer e :junk-allowed t))
                                             line-data))))
              (cadr input-data)))


(defun move-stacks-0 (instruction stacks)
  (let* ((id-src (1- (nth 1 instruction)))
         (id-dst (1- (nth 2 instruction)))
         (n (nth 0 instruction))
         (src-stack (nth id-src stacks))
         (dst-stack (nth id-dst stacks))
         (crates (reverse (subseq src-stack 0 n)))
         (new-src-stack (subseq src-stack n))
         (new-dst-stack (append crates dst-stack))
         (s (copy-seq stacks)))
    (setf (nth id-dst s) new-dst-stack)
    (setf (nth id-src s) new-src-stack)
    s))


(defun move-stacks (instructions stacks)
  (let ((instr (car instructions)))
    (if (not instr)
        stacks
        (move-stacks (cdr instructions) (move-stacks-0 instr stacks)))))


;;; After the rearrangement procedure completes, what crate ends up on top of
;;; each stack?
(mapcar #'first (move-stacks instructions stacks))


;;; --- Part 2 ---
(defun move-stacks-9001 (instruction stacks)
  (let* ((id-src (1- (nth 1 instruction)))
         (id-dst (1- (nth 2 instruction)))
         (n (nth 0 instruction))
         (src-stack (nth id-src stacks))
         (dst-stack (nth id-dst stacks))
         (crates (subseq src-stack 0 n))
         (new-src-stack (subseq src-stack n))
         (new-dst-stack (append crates dst-stack))
         (s (copy-seq stacks)))
    (setf (nth id-dst s) new-dst-stack)
    (setf (nth id-src s) new-src-stack)
    s))


(defun crate-mover-9001 (instructions stacks)
  (let ((instr (car instructions)))
    (if (not instr)
        stacks
        (crate-mover-9001 (cdr instructions) (move-stacks-9001 instr stacks)))))


;;; After the rearrangement procedure completes, what crate ends up on top of
;;; each stack?
(mapcar #'first (crate-mover-9001 instructions stacks))

;;;; -*- mode: Lisp; indent-tabs-mode: nil; -*-
;;;; Advent of Code 2022
;;;;
;;;; --- Day 10 ---
(defpackage :aoc-day-10
  (:use :common-lisp))

(in-package :aoc-day-10)

;;; --- Part 1 ---
;;; load and prepare data
(require 'asdf)
(setq data (mapcar #'(lambda (line)
                       (let ((line-data (uiop:split-string line)))
                         (if (cadr line-data)
                             (list (car line-data)
                                   (parse-integer (cadr line-data)))
                             (list (car line-data)))))
                   (uiop:read-file-lines "input.txt")))
(length data)


(defun noop (cycle register-value)
  (list (list cycle register-value)
        (list (1+ cycle) register-value)))


(defun addx (cycle register-value addend)
  (list (list cycle register-value)
        (list (+ cycle 1) register-value)
        (list (+ cycle 2) (+ register-value addend))))


(defun process-instruction (instruction history)
  (let* ((instr (car instruction))
         (addend (cadr instruction))
         (current-input (car (last history)))
         (cycle (car current-input))
         (value (cadr current-input)))
    (cond ((equal instr "noop")
           (append (butlast history)
                   (noop cycle value)))
          ((equal instr "addx")
           (append (butlast history)
                   (addx cycle value addend))))))


(defun process (instructions history)
  (let ((new-history (process-instruction (car instructions) history)))
    (if (not new-history)
        history
        (process (cdr instructions) new-history))))


;;; Find the signal strength during the 20th, 60th, 100th, 140th, 180th,
;;; and 220th cycles. What is the sum of these six signal strengths?
(let* ((start-values (list (list 1 1)))
       (process-results (process data start-values))
       (cycles (list 20 60 100 140 180 220))
       (sums (mapcar #'(lambda (e) (reduce #'* e))
                     (remove-if #'(lambda (result)
                                    (not (member (car result) cycles)))
                                process-results))))
  (reduce #'+ sums))


;;; --- Part 2 ---
(defun light-pixel (pixel register-value)
  (let ((sprite (list (1- register-value)
                      register-value
                      (1+ register-value))))
    (if (member pixel sprite) #\# #\.)))


(let* ((start-values (list (list 1 1)))
       (process-data (process data start-values))
       (pixels (mapcar #'(lambda (e)
                           (light-pixel (mod (1- (car e)) 40)
                                        (cadr e)))
                       process-data)))
  (labels ((split-pixel-data (x &optional (rows nil))
             (let* ((n (length x))
                    (row (if (>= n 40)
                             (subseq x 0 40)
                             (subseq x 0 n)))
                    (remaining (nthcdr 40 x))
                    (row-list (append rows (list row))))
               (if (not remaining)
                   row-list
                   (split-pixel-data remaining row-list)))))
    (mapcar #'(lambda (row) (concatenate 'string row))
            (split-pixel-data pixels))))

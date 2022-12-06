;;;; -*- mode: Lisp; indent-tabs-mode: nil; -*-
;;;; Advent of Code 2022
;;;;
;;;; --- Day 6: Tuning Trouble ---

;;; --- Part 1 ---
;;; load and prepare data
(require 'asdf)
(setq data (uiop:read-file-line "input.txt"))

;;; How many characters need to be processed before the first start-of-packet
;;; marker is detected?
(defun uniquep (s)
  (equal (length (remove-duplicates s :test #'equal)) (length s)))

(defun chars-until-marker-complete (char-list marker-length start)
  (let* ((end (+ start marker-length))
         (chars (subseq char-list start end)))
    (if (uniquep chars)
        (+ marker-length (search chars char-list :test #'equal))
        (chars-until-marker-complete char-list
                                     marker-length
                                     (1+ start)))))

(chars-until-marker-complete data 4 0)

;;; --- Part 2 ---
;;; How many characters need to be processed before the first start-of-message
;;; marker is detected?
(chars-until-marker-complete data 14 0)

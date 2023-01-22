;;;; -*- mode: Lisp; indent-tabs-mode: nil; -*-
;;;; Advent of Code 2022
;;;;
;;;; --- Day 13: Distress Signal ---
(defpackage :aoc-day-13
  (:use :common-lisp))

(in-package :aoc-day-13)

;;; --- Part 1 ---
;;; helper functions
(require 'asdf)


;;; rewrite input into lisp syntax
(defun rewrite-input (string)
  ;; this is possibly a complicated way to modify strings:
  ;; 1. replace #\, with #\Space, and turn resulting character sequence
  ;;    into a list of lists of single characters.
  ;; 2. replace '(#\[) with "(list " (as list of characters) and
  ;;    '(#\]) with '(#\))
  ;; 3. from the resulting list of lists of characters, construct the
  ;;    result string
  ;; Note: Could probably be solved more elegantly with a reader macro.
  (let* ((no-commas ; remove commas from string
           (mapcar #'list (coerce (substitute #\Space #\, string) 'list)))
         (new-chars ; change "[", "]" to "(list" and ")"
           (subst (list #\))
                  (list #\])
                  (subst (coerce "(list " 'list)
                         (list #\[)
                         no-commas
                         :test #'equal)
                  :test #'equal)))
    (if (> (length new-chars) 0)
        (reduce #'(lambda (x y) (concatenate 'string x y))
                new-chars)
        "blank")))


;;; get pairs from list
(defun get-pairs (lines &optional result)
  (let ((new-result
          (append result (list (list (car lines) (cadr lines)))))
        (new-lines (cddr lines)))
    (if (not new-lines)
        new-result
        (get-pairs new-lines new-result))))


;;; compare pairs recursively
(defun compare-pair (left right)
  (labels ((non-empty-list-p (x) ; differentiate NIL from non-empty-list
             (and (listp x) (> (length x) 0))))
    (cond ((and (integerp left) (integerp right))
           (cond ((< left right) 1)
                 ((> left right) -1)
                 ((= left right) 0)))
          ((and (null left) (non-empty-list-p right)) 1)
          ((and (non-empty-list-p left) (null right)) -1)
          ((and (null left) (null right)) 0)
          ((and (integerp left) (listp right)) (compare-pair (list left) right))
          ((and (listp left) (integerp right)) (compare-pair left (list right)))
          (t (let ((comparison (compare-pair (car left) (car right))))
               (if (not (= comparison 0))
                   comparison
                   (compare-pair (cdr left) (cdr right))))))))


;;; find indices
(defun find-positions (item list &optional (start 0) (acc NIL))
  (let ((first-position (position item list :start start)))
    (if (null first-position)
        acc
        (find-positions item list (1+ first-position)
                        (append acc (list first-position))))))


;;; load and prepare data: load, rewrite, parse and evaluate input
(setf data (let ((input (uiop:read-file-lines "input.txt")))
             (get-pairs
              ;; this may be done more elegantly with a custom reader macro
              ;; instead of rewriting, parsing and evaluating the strings
              (mapcar #'(lambda (l) (eval (read-from-string l)))
                      (remove "blank"
                              (mapcar #'rewrite-input input)
                              :test #'equal)))))


;;; Determine which pairs of packets are already in the right order.
;;; What is the sum of the indices of those pairs?
(let* ((results ; compare pairs
         (mapcar #'(lambda (x) (compare-pair (car x) (cadr x))) data))
       (indices ; find indices of pairs that are already in the correct order
         (find-positions 1 results)))
  ;; compute sum of indices
  (reduce #'+ (mapcar #'1+ indices)))


;;; --- Part 2 ---
;; flatten list of pairs of packets
(defun flatten-list (list &optional (acc NIL))
  (let ((element (car list))
        (new-list (cdr list)))
    (if (null element)
        acc
        (flatten-list new-list (append acc element)))))


;;; Organize all of the packets into the correct order. What is the decoder
;;; key for the distress signal?
(let* ((div-1 (list (list 2))) ; divider packet 1
       (div-2 (list (list 6))) ; divider packet 2
       (packets (append (list div-1) ; packet list
                        (list div-2)
                        (flatten-list data)))
       ;; sort packets
       (sorted (sort (copy-seq packets)
                     #'(lambda (x y) (= (compare-pair x y) 1)))))
  ;; compute decoder key
  (* (1+ (position div-1 sorted :test #'equal))
     (1+ (position div-2 sorted :test #'equal))))
 

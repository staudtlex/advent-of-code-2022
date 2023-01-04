;;;; -*- mode: Lisp; indent-tabs-mode: nil; -*-
;;;; Advent of Code 2022
;;;;
;;;; --- Day 11 ---
(defpackage :aoc-day-11
  (:use :common-lisp))

(in-package :aoc-day-11)

;;; --- Part 1 ---
;;; load and prepare data
(require 'asdf)
(setf data (uiop:read-file-lines "input.txt"))


(defun split-monkey-data (data monkey-infos)
  (let* ((n (length data))
         (info (if (>= n 7)
                   (subseq data 0 7)
                   (subseq data 0)))
         (remaining (nthcdr 7 data))
         (info-list (append monkey-infos (list info))))
    (if (not remaining)
        info-list
        (split-monkey-data remaining info-list))))


(defstruct monkey
  id
  items
  divisor
  operation
  destinations
  inspections)


(defun parse-monkey-data (monkey-data)
  (labels ((keep-digits (s)
             (remove-if #'(lambda (x) (not (digit-char-p x))) s))
           (parse-id (data)
             (parse-integer (keep-digits (nth 0 data))))
           (parse-items (data)
             (let ((item-string
                     (remove-if #'(lambda (x) (not (or (digit-char-p x)
                                                       (equal x #\,))))
                                (nth 1 data))))
               (mapcar #'parse-integer
                       (uiop:split-string item-string :separator '(#\,)))))
           (parse-op (data)
             (let ((value
                     (parse-integer (keep-digits (nth 2 data)) :junk-allowed t)))
               (cond ((find #\+ (nth 2 data))
                      (if value
                          (lambda (old) (+ old value))
                          (lambda (old) (+ old old))))
                     ((find #\* (nth 2 data))
                      (if value
                          (lambda (old) (* old value))
                          (lambda (old) (* old old)))))))
           (parse-divisor (data)
             (parse-integer (keep-digits (nth 3 data))))
           (parse-destinations (data)
             (let ((divisible-by
                     (parse-integer (keep-digits (nth 3 data))))
                   (dest-if-true
                     (parse-integer (keep-digits (nth 4 data))))
                   (dest-if-false
                     (parse-integer (keep-digits (nth 5 data)))))
               (function (lambda (worry-level)
                 (if (= (mod worry-level divisible-by) 0)
                     dest-if-true
                     dest-if-false))))))
    (let ((monkey-id (parse-id monkey-data))
          (items (parse-items monkey-data))
          (divisor (parse-divisor monkey-data))
          (operation (parse-op monkey-data))
          (destinations (parse-destinations monkey-data)))
      (make-monkey :id monkey-id
                   :items items
                   :divisor divisor
                   :operation operation
                   :destinations destinations
                   :inspections 0))))


(defun handle-item (item monkey relief-function)
  (labels ((inspect-item (i)
             (funcall (monkey-operation monkey) i))
           (determine-destination (worry-level)
             (funcall (monkey-destinations monkey) worry-level)))
    (let* ((new-worry-level
             (funcall relief-function (inspect-item item)))
           (dest-monkey
             (determine-destination new-worry-level))
           (from-monkey
             (monkey-id monkey)))
      (pairlis (list 'from-item-value
                     'dest-item-value
                     'from-monkey
                     'dest-monkey)
               (list item
                     new-worry-level
                     from-monkey
                     dest-monkey)))))


(defun add-item (item monkey)
  (make-monkey :id (monkey-id monkey)
               :items (append (monkey-items monkey) (list item))
               :divisor (monkey-divisor monkey)
               :operation (monkey-operation monkey)
               :destinations (monkey-destinations monkey)
               :inspections (monkey-inspections monkey)))


(defun remove-item (item monkey)
  (make-monkey :id (monkey-id monkey)
               :items (remove item (monkey-items monkey) :count 1)
               :divisor (monkey-divisor monkey)
               :operation (monkey-operation monkey)
               :destinations (monkey-destinations monkey)
               :inspections (1+ (monkey-inspections monkey))))


(defun throw-item (item-info monkeys)
  (let ((item-old-level (cdr (assoc 'from-item-value item-info)))
        (item-new-level (cdr (assoc 'dest-item-value item-info)))
        (dest-monkey-id (cdr (assoc 'dest-monkey item-info)))
        (from-monkey-id (cdr (assoc 'from-monkey item-info))))
    (let ((dest-monkey-new
            (add-item item-new-level (nth dest-monkey-id monkeys)))
          (from-monkey-new
            (remove-item item-old-level (nth from-monkey-id monkeys))))
      (substitute-if from-monkey-new
                     #'(lambda (m) (= from-monkey-id
                                      (monkey-id m)))
                     (substitute-if dest-monkey-new
                                    #'(lambda (m) (= dest-monkey-id
                                                     (monkey-id m)))
                                    monkeys)))))


(defun throw-items-and-update (item-infos monkeys)
  (let* ((item-info (car item-infos))
         (monkeys-new (if item-info
                          (throw-item item-info monkeys)
                          nil)))
    (if (not item-info)
        monkeys
        (throw-items-and-update (cdr item-infos) monkeys-new))))


(defun play-round (monkey-list index relief-function)
  (labels ((play-round-0 (monkey monkey-list f)
             (let ((item-infos
                     (mapcar #'(lambda (item) (handle-item item monkey f))
                             (monkey-items monkey))))
               (throw-items-and-update item-infos monkey-list))))
    (let* ((monkey
             (nth index monkey-list))
           (new-monkey-list
             (if monkey
                 (play-round-0 monkey monkey-list relief-function) 
                 nil)))
      (if (not monkey)
          monkey-list
          (play-round new-monkey-list (1+ index) relief-function)))))


(defun play-rounds (game-history number current relief-function)
  (let ((new-history
          (append game-history
                  (list (play-round (car (last game-history))
                                    0
                                    relief-function)))))
    (if (= current number)
        game-history
        (play-rounds new-history number (1+ current) relief-function))))


;;; Figure out which monkeys to chase by counting how many items they inspect
;;; over 20 rounds. What is the level of monkey business after 20 rounds of
;;; stuff-slinging simian shenanigans?
(let ((monkey-data
        (mapcar #'parse-monkey-data (split-monkey-data data nil))))
  (labels ((compute-relief-value (i) (floor i 3)))
    (let ((game-history
            (play-rounds (list monkey-data) 20 0 #'compute-relief-value)))
      (reduce #'* (subseq (sort (mapcar #'monkey-inspections
                                        (car (last game-history)))
                                #'>)
                          0 2)))))


;;; --- Part 2 ---
;;; Worry levels are no longer divided by three after each item is inspected;
;;; you'll need to find another way to keep your worry levels manageable.
;;; Starting again from the initial state in your puzzle input, what is the
;;; level of monkey business after 10000 rounds?
(let* ((monkey-data
         (mapcar #'parse-monkey-data (split-monkey-data data nil)))
       (divisor
         (reduce #'* (mapcar #'monkey-divisor monkey-data))))
  ;; Use (mod a b) with b being the least common multiple of the monkey's
  ;; divisors.
  (labels ((new-relief-function (i) (mod i divisor)))
    (let ((game-history
            (play-rounds (list monkey-data) 10000 0 #'new-relief-function)))
      (reduce #'* (subseq (sort (mapcar #'monkey-inspections
                                        (car (last game-history)))
                                #'>)
                          0 2)))))

;;; Thanks to reddit for hinting at using (mod a b) https://www.reddit.com/r/adventofcode/comments/zih7gf/2022_day_11_part_2_what_does_it_mean_find_another/

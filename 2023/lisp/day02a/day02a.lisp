#!/usr/bin/env clisp

(setf *progname* "day02a.lisp")

(load "~/quicklisp/setup.lisp")
(ql:quickload "split-sequence")

(setf *total-red* 12)
(setf *total-green* 13)
(setf *total-blue* 14)

(defun prog-usage ()
  (format t "usage: ~a <file>" *progname*)
  (exit))

(defun read-file-into-lines (filename)
  (with-open-file (file filename :direction :input)
    (setf lines
      (loop for line = (read-line file nil)
          while line
          collect line))
    (close file))
  lines)

(defun process (filename)
  (setf result 0)
  (setf lines (read-file-into-lines filename))
  (dolist (line lines)
    (setf valid t)
    (setf game-part (car (split-sequence:split-sequence #\: line)))
    (setf game-id
      (parse-integer
        (string-trim " " (cadr (split-sequence:split-sequence #\Space game-part)))))
    (setf draws-part (cadr (split-sequence:split-sequence #\: line)))
    (setf draws (split-sequence:split-sequence #\; draws-part))
    (dolist (draw draws)
      (setf color-amounts (split-sequence:split-sequence #\, draw))
      (dolist (color-amount-part color-amounts)
        (setf color-amount
          (split-sequence:split-sequence #\Space (string-trim " " color-amount-part)))
        (setf amount (parse-integer (car color-amount)))
        (setf color (cadr color-amount))
        (cond ((string= color "red")
                (if (> amount *total-red*)
                    (setf valid nil)))
              ((string= color "green")
                (if (> amount *total-green*)
                    (setf valid nil)))
              ((string= color "blue")
                (if (> amount *total-blue*)
                    (setf valid nil))))))
    (if (not (null valid))
      (setf result (+ game-id result))))
  result)

(defun main ()
  (if (< (length *args*) 1)
    (prog-usage))
  (setf filename (car *args*))
  (setf result (process filename))
  (format t "result = ~d" result))

(main)

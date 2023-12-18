#!/usr/bin/env clisp

(setf *progname* "day01a.lisp")

(defun prog-usage ()
  (format t "usage: ~a <file>" *progname*)
  (exit))

(defun process (filename)
  (setf sum 0)
  (setf digits '("0" "1" "2" "3" "4" "5" "6" "7" "8" "9"))
  (with-open-file (file filename :direction :input)
    (loop for line = (read-line file nil)
      while line do
      (progn 
        (setq min-index nil)
        (setq max-index nil)
        (setq left-digit nil)
        (setq right-digit nil))
        (loop for digit in digits do
          (progn
            (setf left-index (search digit line))
            (setf right-index (search digit line :from-end t))
            (if (and (not (null left-index))
                     (or (null min-index)
                         (< left-index min-index)))
              (progn
                (setf min-index left-index)
                (setf left-digit digit)))
            (if (and (not (null right-index))
                     (or (null max-index)
                         (> right-index max-index)))
              (progn
                (setf max-index right-index)
                (setf right-digit digit)))))
        (setf numstr (format nil "~a~a" left-digit right-digit))
        (setf sum (+ sum (parse-integer numstr))))
    (close file))
  sum)

(defun main ()
  (if (< (length *args*) 1)
    (prog-usage))
  (setf filename (car *args*))
  (setf result (process filename))
  (format t "result = ~d" result))

(main)

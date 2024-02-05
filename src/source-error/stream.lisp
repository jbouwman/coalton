(in-package #:source-error)

;; Functions for random access to lines in a positionable stream.

(defun line-offsets (stream &optional limit)
  "Compute the offsets of lines in a stream."
  (file-position stream 0)
  (loop :with index := 0
        :for char := (read-char stream nil nil)
        :unless (and char (or (not limit)
                              (<= index limit)))
          :return offsets
        :else
          :when (char= char #\Newline)
            :collect (1+ index) :into offsets
        :do (incf index)))

(defun resolve-offsets (line-offsets offsets)
  "Given the positions of newlines in a stream, compute the line and
column numbers for a sequence of absolute stream offsets."
  (loop :with line := 1
        :with position := 0
        :while offsets
        :when (or (null line-offsets)
                  (< (car offsets)
                     (car line-offsets)))
          :collect (list (car offsets) line (- (car offsets) position))
          :and :do (pop offsets)
        :else
          :do (setf position (car line-offsets)
                    line (1+ line)
                    line-offsets (cdr line-offsets))))

(defun line-at-offset (stream offset)
  (file-position stream offset)
  (read-line stream))

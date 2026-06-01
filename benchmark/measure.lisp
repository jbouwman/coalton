;;;; Cost measurement: wall-clock and allocation, isolated per phase.

(in-package #:coalton-benchmark)

(defun measure (thunk)
  "Call THUNK and return (values RESULT SECONDS BYTES-CONSED)."
  (let ((t0 (get-internal-run-time))
        (b0 (sb-ext:get-bytes-consed)))
    (let ((result (funcall thunk)))
      (values result
              (/ (float (- (get-internal-run-time) t0) 1.0d0)
                 internal-time-units-per-second)
              (- (sb-ext:get-bytes-consed) b0)))))

(defun median (numbers)
  (let* ((sorted (sort (copy-seq numbers) #'<))
         (n (length sorted)))
    (if (zerop n)
        0
        (if (oddp n)
            (elt sorted (floor n 2))
            (/ (+ (elt sorted (1- (floor n 2)))
                  (elt sorted (floor n 2)))
               2)))))

(defun repeat-measure (n thunk)
  "Run THUNK N times, returning a plist with :min-seconds :median-seconds
and :bytes (allocation is deterministic enough to report once). A full GC
runs before the loop to keep timing out of the previous run's collection."
  (sb-ext:gc :full t)
  (let ((times '())
        (bytes 0))
    (dotimes (i n)
      (multiple-value-bind (result seconds consed) (measure thunk)
        (declare (ignore result))
        (push seconds times)
        (setf bytes consed)))            ; last run's allocation
    (list :min-seconds (reduce #'min times)
          :median-seconds (median times)
          :bytes bytes
          :iterations n)))

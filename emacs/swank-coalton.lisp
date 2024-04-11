;;; swank-coalton.lisp

(in-package :swank)

(defun system-loaded-p (system-designator)
  (find system-designator (asdf:already-loaded-systems)
        :test #'string=))

(defun system-available-p (system-designator)
  (asdf:find-system system-designator))

(defun system-status (system-designator)
  (cond ((system-loaded-p system-designator)
         :loaded)
        ((system-available-p system-designator)
         :available)
        (t
         :unavailable)))


(defslimefun swank-coalton-status ()
  (system-status "coalton"))

(defslimefun swank-coalton-init ()
  (asdf:load-system "coalton"))


(defslimefun swank-coalton--ast-file (text)
  (with-input-from-string (stream text)
    (coalton-impl/compiler::generate-ast stream)))

(defun compile-and-load (text)
  (with-input-from-string (stream text)
    (uiop:with-temporary-file (:stream lisp-stream
                               :pathname lisp-text
                               :suffix "lisp"
                               :direction :output
                               :keep t)
      (declare (ignore lisp-text))
      (coalton-impl/compiler::%compile-file stream lisp-stream)))
  "ok")

(defslimefun swank-coalton--compile-file (text)
  (compile-and-load text))

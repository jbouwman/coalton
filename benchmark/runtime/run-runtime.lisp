;;;; Runtime micro-benchmark: quantify the struct field-access penalty in
;;;; Coalton's generated code, and compare against codegen variants that
;;;; remove each source of overhead.
;;;;
;;;; Usage (coalton repo root, inside `nix develop`):
;;;;   sbcl --script benchmark/runtime/run-runtime.lisp
;;;;
;;;; The benchmark is a tail loop that, each iteration, destructures a
;;;; two-field Point and sums its fields. All variants do the same
;;;; (generic-integer) arithmetic, so the difference isolates field access.
;;;;
;;;;   v-coalton : the actual Coalton-compiled point-sum-loop (current
;;;;               codegen: typep guard + field reads through the
;;;;               global-lexical reader functions).
;;;;   v-typed   : same struct, but read fields via the defstruct accessor
;;;;               with the concrete type declared, and no typep guard --
;;;;               i.e. what codegen could emit at a statically-typed,
;;;;               single-constructor match site. SBCL open-codes these to
;;;;               instance-refs.
;;;;   v-ideal   : no struct at all; the two values are passed directly
;;;;               (the result of scalar-replacement / deforestation).

;;; Each of these is a separate top-level form so that package-qualified
;;; symbols (coalton-impl/entry, the program package) are only read after
;;; the form that creates the package has been evaluated.
(require :asdf)
(asdf:load-system :coalton)
(coalton-impl/entry:compile
 (coalton-impl/source:make-source-file #p"benchmark/runtime/point-loop.coal")
 :load t)

;;; Stay in a CL package; the Coalton program package does not use CL, so
;;; reference its (internal, unexported) symbols through the rp nickname.
(defpackage #:coalton-runtime-bench
  (:use #:cl)
  (:local-nicknames (#:rp #:coalton-benchmark/runtime-point)))
(in-package #:coalton-runtime-bench)

(declaim (type fixnum +iterations+))
(defparameter +iterations+ 100000000)

;;; v-typed: direct, open-codable field access at a known concrete type,
;;; no constructor dispatch. This is what codegen could emit when the
;;; matched value's type (and single constructor) is statically known.
(defun v-typed (n p acc)
  (declare (type integer n acc)
           (type rp::point/point p)
           (optimize (speed 3) (safety 0)))
  (if (= n 0)
      acc
      (v-typed (- n 1) p
               (+ acc (+ (rp::point/point-_0 p) (rp::point/point-_1 p))))))

;;; v-ideal: no aggregate at all; the fields travel as plain values.
(defun v-ideal (n x y acc)
  (declare (type integer n acc x y)
           (optimize (speed 3) (safety 0)))
  (if (= n 0)
      acc
      (v-ideal (- n 1) x y (+ acc (+ x y)))))

;;; Timing

(defun bench (thunk &optional (reps 3))
  (sb-ext:gc :full t)
  (let ((best most-positive-fixnum))
    (dotimes (i reps)
      (let ((t0 (get-internal-run-time)))
        (funcall thunk)
        (setf best (min best (- (get-internal-run-time) t0)))))
    (/ (float best 1.0d0) internal-time-units-per-second)))

(let* ((p (rp::point 3 4))
       (n +iterations+))
  (flet ((ms (s) (* 1000.0d0 s)))
    (let ((c (bench (lambda () (rp::point-sum-loop n p 0))))
          (ty (bench (lambda () (v-typed n p 0))))
          (id (bench (lambda () (v-ideal n 3 4 0)))))
      (format t "~&~%Runtime field-access micro-benchmark (~:D iterations, min of 3)~%" n)
      (format t "~60,,,'-<~>~%")
      (format t "~28A ~12@A ~14@A~%" "variant" "ms" "ns/iter")
      (format t "~60,,,'-<~>~%")
      (flet ((row (name s)
               (format t "~28A ~12,1F ~14,2F~%"
                       name (ms s) (/ (* s 1.0d9) n))))
        (row "v-coalton (current)" c)
        (row "v-typed (open-coded read)" ty)
        (row "v-ideal (no struct)" id))
      (format t "~60,,,'-<~>~%")
      (format t "current vs typed:  ~,2Fx~%" (/ c ty))
      (format t "current vs ideal:  ~,2Fx~%" (/ c id))))
  (sb-ext:exit :code 0))

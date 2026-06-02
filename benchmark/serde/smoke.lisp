;;;; Smoke test for the live edit-IR wiring: compile a small program to Lisp
;;;; source (which now emits decode-* replay forms), then load that source
;;;; into a fresh-ish image and confirm the definitions are usable.
(require :asdf)
(asdf:load-system :coalton)

(in-package :cl-user)

(defparameter *src* "
(package coalton-serde-smoke
  (import coalton-prelude))

(define (double x) (the Integer (+ x x)))

(define-type (Tree :a) (Leaf :a) (Branch (Tree :a) (Tree :a)))

(declare tree-size (Tree :a -> Integer))
(define (tree-size t)
  (match t
    ((Leaf _) 1)
    ((Branch l r) (+ (tree-size l) (tree-size r)))))
")

(let* ((lisp (with-output-to-string (out)
               (coalton-impl/entry:compile-to-lisp
                (coalton-impl/source:make-source-string *src*) out))))
  (format t "~&;; generated replay+program source: ~D chars~%" (length lisp))
  ;; Show that the value-type replay now goes through the decoder.
  (format t ";; mentions serde decoder: ~A~%"
          (and (search "decode-scheme" lisp) t))
  (format t ";; mentions decode-type-entry: ~A~%"
          (and (search "decode-type-entry" lisp) t))
  ;; Load the generated source: this compiles and evaluates the replay forms,
  ;; exercising decode at load time.
  (with-input-from-string (in lisp)
    (load in))
  (format t ";; loaded generated source OK~%"))

(format t ";; SMOKE OK~%")
(finish-output)
(sb-ext:exit :code 0)

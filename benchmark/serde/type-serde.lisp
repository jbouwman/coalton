;;;; Prototype of the edit-IR serialization boundary, for the type layer.
;;;;
;;;; GOAL-025 step 4 (see docs/internals/design-docs/environment-replay-and-an-edit-ir.md):
;;;; the environment replay should serialize edits as representation-independent
;;;; data instead of embedding compiler-internal structs. This file prototypes
;;;; the core of that boundary -- encode/decode for kinds, types, predicates,
;;;; and type schemes -- as plain s-expressions, and validates it by
;;;; round-tripping every value-type scheme in the loaded stdlib environment
;;;; against ty-scheme= (alpha-equivalence).
;;;;
;;;; Load on top of :coalton, then call (coalton-serde:roundtrip-report).
;;;; Internal (::) access is used deliberately: this is the one place that is
;;;; allowed to know the internal representation -- the whole point of the
;;;; boundary.

(defpackage #:coalton-serde
  (:use #:cl)
  (:local-nicknames
   (#:ty #:coalton-impl/typechecker/types)
   (#:kinds #:coalton-impl/typechecker/kinds)
   (#:scheme #:coalton-impl/typechecker/scheme)
   (#:pred #:coalton-impl/typechecker/predicate)
   (#:tcenv #:coalton-impl/typechecker/environment)
   (#:algo #:coalton-impl/algorithm)
   (#:entry #:coalton-impl/entry))
  (:export #:encode-scheme #:decode-scheme
           #:encode-type #:decode-type
           #:roundtrip-report #:size-report))

(in-package #:coalton-serde)

;;; Kinds: *  |  (-> kind kind)

(defun encode-kind (k)
  (typecase k
    (kinds::kstar '*)
    (kinds::kfun (list '-> (encode-kind (kinds::kfun-from k))
                            (encode-kind (kinds::kfun-to k))))
    (t (error "unencodable kind: ~S" k))))

(defun decode-kind (form)
  (cond
    ((eq form '*) kinds::+kstar+)
    ((and (consp form) (eq (first form) '->))
     (kinds::make-kfun :from (decode-kind (second form))
                       :to (decode-kind (third form))))
    (t (error "undecodable kind: ~S" form))))

;;; Types

(defun encode-keywords (entries open-p)
  (if (and (null entries) (not open-p))
      '()
      (list :keys (mapcar (lambda (e)
                            (list (ty::keyword-ty-entry-keyword e)
                                  (encode-type (ty::keyword-ty-entry-type e))))
                          entries)
            :open open-p)))

(defun decode-keywords (form)
  (if (null form)
      (values '() nil)
      (destructuring-bind (&key keys open) form
        (values (mapcar (lambda (pair)
                          (ty::make-keyword-ty-entry
                           :keyword (first pair)
                           :type (decode-type (second pair))))
                        keys)
                open))))

(defun encode-type (type)
  (typecase type
    (ty::tycon  (list 'con (ty::tycon-name type) (encode-kind (ty::tycon-kind type))))
    (ty::tgen   `(gen ,(ty::tgen-id type)
                      ,@(when (ty::tgen-allow-result-p type) '(:result))))
    (ty::tapp   (list 'app (encode-type (ty::tapp-from type))
                           (encode-type (ty::tapp-to type))))
    (ty::function-ty
     (list 'fn
           (mapcar #'encode-type (ty::function-ty-positional-input-types type))
           (encode-keywords (ty::function-ty-keyword-input-types type)
                            (ty::function-ty-keyword-open-p type))
           (mapcar #'encode-type (ty::function-ty-output-types type))))
    (ty::result-ty
     (list 'result (mapcar #'encode-type (ty::result-ty-output-types type))))
    (ty::tyvar  `(var ,(ty::tyvar-id type) ,(encode-kind (ty::tyvar-kind type))
                      ,@(when (ty::tyvar-allow-result-p type) '(:result))))
    (t (error "unencodable type: ~S" type))))

(defun decode-type (form)
  (ecase (first form)
    (con (ty::make-tycon :name (second form) :kind (decode-kind (third form))))
    (gen (ty::make-tgen :id (second form)
                        :allow-result-p (and (member :result (cddr form)) t)))
    (app (ty::make-tapp :from (decode-type (second form))
                        :to (decode-type (third form))))
    (fn (destructuring-bind (ins kw outs) (rest form)
          (multiple-value-bind (entries open-p) (decode-keywords kw)
            (ty::make-function-ty
             :positional-input-types (mapcar #'decode-type ins)
             :keyword-input-types entries
             :keyword-open-p open-p
             :output-types (mapcar #'decode-type outs)))))
    (result (ty::make-result-ty :output-types (mapcar #'decode-type (second form))))
    (var (ty::make-tyvar :id (second form) :kind (decode-kind (third form))
                         :allow-result-p (and (member :result (cdddr form)) t)))))

;;; Predicates and schemes

(defun encode-pred (p)
  (list 'pred (pred::ty-predicate-class p)
        (mapcar #'encode-type (pred::ty-predicate-types p))))

(defun decode-pred (form)
  (pred::make-ty-predicate :class (second form)
                           :types (mapcar #'decode-type (third form))))

(defun encode-scheme (s)
  (let ((q (scheme::ty-scheme-type s)))
    (list 'scheme
          (scheme::ty-scheme-explicit-p s)
          (mapcar #'encode-kind (scheme::ty-scheme-kinds s))
          (mapcar #'encode-pred (pred::qualified-ty-predicates q))
          (encode-type (pred::qualified-ty-type q)))))

(defun decode-scheme (form)
  (destructuring-bind (tag explicit kinds-form preds-form type-form) form
    (declare (ignore tag))
    (scheme::make-ty-scheme
     :explicit-p explicit
     :kinds (mapcar #'decode-kind kinds-form)
     :type (pred::make-qualified-ty
            :predicates (mapcar #'decode-pred preds-form)
            :type (decode-type type-form)))))

;;; Round-trip the entire stdlib value environment against ty-scheme=.

(defun roundtrip-report ()
  (let* ((env entry::*global-environment*)
         (schemes (algo::immutable-map-values
                   (tcenv::environment-value-environment env)))
         (total 0) (ok 0) (mismatch '()) (errored '()))
    (dolist (s schemes)
      (incf total)
      (handler-case
          (let ((round (decode-scheme (encode-scheme s))))
            (if (scheme::ty-scheme= s round)
                (incf ok)
                (push s mismatch)))
        (error (e) (push (cons s e) errored))))
    (format t "~&edit-IR type serde round-trip over stdlib value schemes~%")
    (format t "~60,,,'-<~>~%")
    (format t "schemes:   ~D~%" total)
    (format t "round-trip ok (ty-scheme=): ~D~%" ok)
    (format t "mismatch:  ~D~%" (length mismatch))
    (format t "errored:   ~D~%" (length errored))
    (when errored
      (format t "~%first errors:~%")
      (dolist (e (subseq errored 0 (min 5 (length errored))))
        (format t "  ~A~%" (cdr e))))
    (when mismatch
      (format t "~%first mismatch (original vs round-tripped encoding):~%")
      (let ((s (first mismatch)))
        (format t "  orig:  ~S~%  round: ~S~%"
                (encode-scheme s)
                (encode-scheme (decode-scheme (encode-scheme s))))))
    ;; A couple of sample encodings, to show the IR is plain data.
    (format t "~%sample encodings:~%")
    (dolist (s (subseq schemes 0 (min 3 (length schemes))))
      (format t "  ~S~%" (encode-scheme s)))
    (values total ok (length mismatch) (length errored))))

;;; Size comparison: the IR encoding vs the current replay representation
;;; (the readably-printed struct, which is what runtime-quote + make-load-form
;;; emit). Printed the same way the replay is (downcase, circle on), so the
;;; comparison reflects emitted bytes.

(defun replay-chars (obj)
  (with-standard-io-syntax
    (let ((*print-case* :downcase) (*print-circle* t) (*print-readably* t)
          (*print-right-margin* 80))
      (length (prin1-to-string obj)))))

(defun ir-chars (form)
  (with-standard-io-syntax
    (let ((*print-case* :downcase) (*print-circle* t) (*print-right-margin* 80))
      (length (prin1-to-string form)))))

(defun size-report ()
  (let* ((env entry::*global-environment*)
         (schemes (algo::immutable-map-values
                   (tcenv::environment-value-environment env)))
         (ir 0) (replay 0))
    (dolist (s schemes)
      (incf ir (ir-chars (encode-scheme s)))
      (incf replay (replay-chars s)))
    (format t "~&edit-IR type serde size, over ~D stdlib value schemes~%" (length schemes))
    (format t "~60,,,'-<~>~%")
    (format t "current replay (readable #S struct): ~:D chars~%" replay)
    (format t "edit-IR encoding:                    ~:D chars~%" ir)
    (format t "ratio (replay / IR):                 ~,2Fx~%" (/ (float replay) ir))
    (values replay ir)))

;;;; Correctness oracles.
;;;;
;;;; Each oracle turns a phase's output into a stable, human-diffable text
;;;; artifact. A later refactor that changes parser AST shape, inferred
;;;; types, or generated code shows up as a diff against the pinned artifact.

(in-package #:coalton-benchmark)

;;; Parse oracle (first cut): counts per top-level form kind. Full
;;; parser-AST pinning is PLAN-308 stage 7.

(defun parse-summary (program)
  (list :types (length (parser:program-types program))
        :type-aliases (length (parser:program-type-aliases program))
        :structs (length (parser:program-structs program))
        :classes (length (parser:program-classes program))
        :instances (length (parser:program-instances program))
        :declares (length (parser:program-declares program))
        :defines (length (parser:program-defines program))
        :specializations (length (parser:program-specializations program))
        :lisp-forms (length (parser:program-lisp-forms program))))

;;; Typecheck oracle: the inferred top-level value signatures, rendered as
;;; strings via the compiler's own type printer. Stable across codegen
;;; changes; this is the primary inference-regression guard.

(defun inferred-signatures (tcr)
  "Return a sorted list of (name-string . type-string) for the defines in
the typechecked program."
  (let* ((program (tc-result-program tcr))
         (env (tc-result-env tcr))
         (pairs '()))
    (dolist (define (parser:program-defines program))
      (let* ((name (parser:node-variable-name (parser:toplevel-define-name define)))
             (scheme (tc:lookup-value-type env name :no-error t)))
        (push (cons (string name)
                    (if scheme (tc:type-to-string scheme env) "<unresolved>"))
              pairs)))
    (sort pairs #'string< :key #'car)))

;;; Codegen oracle: the generated Lisp forms, printed to text with
;;; uninterned (gensym) symbols renamed to stable placeholders by order of
;;; first appearance, so the artifact is deterministic across runs.

(defun gentemp-name-p (name)
  "True for a renamer/gentemp name shape `<base>-<digits>` (src/parser/
renamer.lisp uses gentemp with a never-resetting global counter, so these
suffixes vary run to run)."
  (let ((dash (position #\- name :from-end t)))
    (and dash
         (< (1+ dash) (length name))
         (every #'digit-char-p (subseq name (1+ dash))))))

(defun generated-symbol-p (x program-package)
  "True for symbols whose identity is an artifact of compilation rather than
source: uninterned gensyms, and gentemp-renamed locals interned in the
program's own package. Real top-level names and library symbols are kept."
  (and (symbolp x)
       (or (null (symbol-package x))
           (and program-package
                (eq (symbol-package x) program-package)
                (gentemp-name-p (symbol-name x))))))

(defun normalize-gensyms (form program-package)
  "Copy FORM, replacing each generated symbol with a stable G<n> symbol keyed
by first-seen order. Source-meaningful symbols, literals, and structure are
preserved, so the rendered text is deterministic across runs but still
diffs on real codegen changes."
  (let ((table (make-hash-table :test #'eq))
        (counter 0))
    (labels ((walk (x)
               (cond
                 ((generated-symbol-p x program-package)
                  (or (gethash x table)
                      (setf (gethash x table)
                            (make-symbol (format nil "G~D" (incf counter))))))
                 ((consp x)
                  (cons (walk (car x)) (walk (cdr x))))
                 ((and (vectorp x) (not (stringp x)))
                  (map 'vector #'walk x))
                 (t x))))
      (walk form))))

(defun codegen-text (form program-package)
  "Render the codegen output FORM as deterministic text."
  (with-standard-io-syntax
    (let ((*package* (find-package "CL"))
          (*print-case* ':downcase)
          (*print-circle* t)
          (*print-pretty* t)
          (*print-right-margin* 80)
          (*print-readably* nil))
      ;; Coerce to a general character string: an all-base-char string is
      ;; serialized by prin1 as the non-portable #A(...) form, which does
      ;; not round-trip through the standard reader and so breaks the
      ;; baseline diff. A (simple-array character) prints as plain "...".
      (coerce (prin1-to-string (normalize-gensyms form program-package))
              '(simple-array character (*))))))

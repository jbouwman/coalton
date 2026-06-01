;;;; Runner: drive the corpus through the staged compiler, capture per-phase
;;;; correctness artifacts and cost, and pin / diff against a stored baseline.

(in-package #:coalton-benchmark)

(defparameter *default-iterations* 5
  "Timed iterations per phase per file. Lower this for very large inputs.")

(defun corpus-directory ()
  (asdf:system-relative-pathname "coalton-benchmark" "corpus/"))

(defun corpus-files ()
  (sort (directory (merge-pathnames "*.coal" (corpus-directory)))
        #'string< :key #'namestring))

(defun measure-isolated (n prep phase)
  "Run N iterations. Each calls (PREP) untimed to build a fresh input, then
times (PHASE input) alone. Returns a cost plist. Isolating the phase this
way avoids re-using a program the typecheck phase has mutated in place."
  (sb-ext:gc :full t)
  (let ((times '())
        (bytes 0))
    (dotimes (i n)
      (let ((input (funcall prep)))
        (multiple-value-bind (result seconds consed)
            (measure (lambda () (funcall phase input)))
          (declare (ignore result))
          (push seconds times)
          (setf bytes consed))))
    (list :min-seconds (reduce #'min times)
          :median-seconds (median times)
          :bytes bytes
          :iterations n)))

(defun benchmark-file (path &key (iterations *default-iterations*))
  "Drive one corpus file through all phases, returning a result plist.
On a compiler error, returns (:name ... :error \"...\")."
  (let ((name (file-namestring path)))
    (handler-case
        (let* ((source (source:make-source-file path))
               ;; Bind *package* to the program's package for the whole
               ;; pipeline, as file compilation does. Codegen interns
               ;; counter-named temporaries into the ambient *package*; if
               ;; that drifts (script vs repl) the generated text is
               ;; nondeterministic, so pin it to the program package, where
               ;; normalize-gensyms catches those temporaries.
               (program-package (parser:program-lisp-package (parse-source source)))
               (*package* program-package)
               ;; Correctness pass (one run, artifacts captured).
               (program (parse-source source))
               (summary (parse-summary program))
               (tcr (typecheck-program program))
               (signatures (inferred-signatures tcr))
               (codegen-form (codegen-tc-result tcr))
               (gen-text (codegen-text codegen-form program-package)))
          ;; Cost pass (isolated, repeated). Re-parse / re-typecheck per
          ;; iteration so each phase times fresh, unmutated input.
          (let ((parse-cost
                  (measure-isolated iterations
                                    (lambda () source)
                                    (lambda (src) (parse-source src))))
                (typecheck-cost
                  (measure-isolated iterations
                                    (lambda () (parse-source source))
                                    (lambda (prog) (typecheck-program prog))))
                (codegen-cost
                  (measure-isolated iterations
                                    (lambda () (typecheck-program (parse-source source)))
                                    (lambda (tcr) (codegen-tc-result tcr)))))
            (list :name name
                  :parse-summary summary
                  :signatures signatures
                  :codegen-text gen-text
                  :cost (list :parse parse-cost
                              :typecheck typecheck-cost
                              :codegen codegen-cost))))
      (error (e)
        (list :name name :error (princ-to-string e))))))

(defun run-baseline (&key (iterations *default-iterations*))
  "Drive the whole corpus, returning a list of per-file result plists."
  (let ((files (corpus-files)))
    (when (null files)
      (warn "No corpus files found in ~A" (corpus-directory)))
    (mapcar (lambda (p) (benchmark-file p :iterations iterations)) files)))

;;; Reporting

(defun ms (seconds) (* 1000.0d0 seconds))

(defun print-report (results &optional (stream *standard-output*))
  (format stream "~&~%Coalton per-phase baseline (median ms / MB consed)~%")
  (format stream "~70,,,'-<~>~%")
  (format stream "~24A ~12@A ~12@A ~12@A~%" "file" "parse" "typecheck" "codegen")
  (format stream "~70,,,'-<~>~%")
  (dolist (r results)
    (if (getf r :error)
        (format stream "~24A  ERROR: ~A~%" (getf r :name) (getf r :error))
        (let ((c (getf r :cost)))
          (flet ((cell (phase)
                   (let ((p (getf c phase)))
                     (format nil "~,2F/~,1F"
                             (ms (getf p :median-seconds))
                             (/ (getf p :bytes) 1048576.0d0)))))
            (format stream "~24A ~12@A ~12@A ~12@A~%"
                    (getf r :name) (cell :parse) (cell :typecheck) (cell :codegen))))))
  (format stream "~70,,,'-<~>~%"))

;;; Pinning and diffing

(defun result-correctness (r)
  "The subset of a result that is a correctness pin (cost excluded)."
  (list :name (getf r :name)
        :error (getf r :error)
        :parse-summary (getf r :parse-summary)
        :signatures (getf r :signatures)
        :codegen-text (getf r :codegen-text)))

(defun write-baseline (results path)
  "Write RESULTS (correctness pins + cost) to PATH as a readable sexp."
  (with-open-file (out path :direction :output
                            :if-exists :supersede :if-does-not-exist :create)
    (with-standard-io-syntax
      (let ((*print-case* :downcase)
            (*print-readably* nil))
        ;; *print-readably* nil keeps strings as plain quoted text
        ;; (human-diffable) while still round-tripping through read.
        (prin1 results out)
        (terpri out))))
  path)

(defun read-baseline (path)
  (with-open-file (in path)
    (with-standard-io-syntax
      (read in))))

(defun find-result (name results)
  (find name results :key (lambda (r) (getf r :name)) :test #'string=))

(defun diff-baseline (path &key (iterations *default-iterations*))
  "Run the corpus fresh and compare correctness pins against the baseline at
PATH. Returns T when everything matches; prints each mismatch. Cost is
reported but never fails the diff."
  (let* ((pinned (read-baseline path))
         (fresh (run-baseline :iterations iterations))
         (ok t))
    (dolist (f fresh)
      (let* ((name (getf f :name))
             (p (find-result name pinned)))
        (cond
          ((null p)
           (setf ok nil)
           (format t "~&NEW FILE (not in baseline): ~A~%" name))
          ((not (equal (result-correctness f) (result-correctness p)))
           (setf ok nil)
           (format t "~&MISMATCH: ~A~%" name)
           (unless (equal (getf f :parse-summary) (getf p :parse-summary))
             (format t "  parse-summary: ~S -> ~S~%"
                     (getf p :parse-summary) (getf f :parse-summary)))
           (unless (equal (getf f :signatures) (getf p :signatures))
             (format t "  signatures differ~%"))
           (unless (equal (getf f :codegen-text) (getf p :codegen-text))
             (format t "  codegen output differs~%"))
           (unless (equal (getf f :error) (getf p :error))
             (format t "  error: ~S -> ~S~%" (getf p :error) (getf f :error)))))))
    (dolist (p pinned)
      (unless (find-result (getf p :name) fresh)
        (setf ok nil)
        (format t "~&MISSING FILE (in baseline, not run): ~A~%" (getf p :name))))
    (print-report fresh)
    (if ok
        (format t "~&BASELINE OK: correctness matches.~%")
        (format t "~&BASELINE DIFF: correctness changed (see above).~%"))
    ok))

(defun main ()
  "Script entry point: run the corpus, print the report, and write a
baseline sexp. Path defaults to baselines/baseline.sexp; pass an alternate
as the first command-line argument."
  (let* ((args (uiop:command-line-arguments))
         (path (if args
                   (pathname (first args))
                   (asdf:system-relative-pathname
                    "coalton-benchmark" "baselines/baseline.sexp")))
         (results (run-baseline)))
    (print-report results)
    (write-baseline results path)
    (format t "~&Wrote baseline to ~A~%" path)
    results))

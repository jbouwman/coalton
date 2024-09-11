(defpackage #:coalton-impl/reader
  (:use
   #:cl)
  (:local-nicknames
   (#:cst #:concrete-syntax-tree)
   (#:codegen #:coalton-impl/codegen)
   (#:settings #:coalton-impl/settings)
   (#:util #:coalton-impl/util)
   (#:parser #:coalton-impl/parser)
   (#:tc #:coalton-impl/typechecker)
   (#:entry #:coalton-impl/entry)))

(in-package #:coalton-impl/reader)

(defvar *coalton-reader-allowed* t
  "Is the Coalton reader allowed to parse the current input?
Used to forbid reading while inside quasiquoted forms.")

(defvar *source* nil
  "The source from which program text is being read.
This symbol may be bound to a string source in the case of direct evaluation in a repl.")

(defun probe-symbol (package-name symbol-name)
  "Look up SYMBOL-NAME in PACKAGE-NAME, returning its value if the package exists and the symbol is bound."
  (let ((package (find-package package-name)))
    (when package
      (let ((symbol (find-symbol symbol-name package)))
        (when (boundp symbol)
          (symbol-value symbol))))))

(defun source-filename ()
  "Return the name of the file from which program text is being compiled or loaded."
  (or *compile-file-truename*
      *load-truename*))

(defun buffer-name ()
  "Return the name of the Emacs buffer containing evaluated or compiled program text."
  (or #+:sbcl (getf (probe-symbol "SB-C" "*SOURCE-PLIST*") ':emacs-buffer)
      (source-filename)
      "repl input"))

(defun read-coalton-toplevel-open-paren (stream char)
  (declare (optimize (debug 2)))

  (unless *coalton-reader-allowed*
    (return-from read-coalton-toplevel-open-paren
      (funcall (get-macro-character #\( (named-readtables:ensure-readtable :standard)) stream char)))

  (parser:with-reader-context stream
    (let* ((source (or *source*
                       (coalton-impl/source:make-source-file (source-filename)
                                                             :name (buffer-name))))
           (first-form
             (multiple-value-bind (form presentp)
                 (parser:maybe-read-form stream source)
               (unless presentp
                 (return-from read-coalton-toplevel-open-paren
                   nil))
               form)))

      (case (cst:raw first-form)
        (coalton:coalton-toplevel
          (entry:compile-coalton-toplevel (parser:read-program stream source ':macro)))

        (coalton:coalton-codegen
          (let ((settings:*emit-type-annotations* nil))
            `',(entry:entry-point (parser:read-program stream source ':macro))))

        (coalton:coalton-codegen-types
          (let ((settings:*emit-type-annotations* t))
            `',(entry:entry-point (parser:read-program stream source ':macro))))

        (coalton:coalton-codegen-ast
          (let* ((settings:*emit-type-annotations* nil)
                 (ast nil)
                 (codegen:*codegen-hook* (lambda (op &rest args)
                                           (when (eql op ':AST)
                                             (push args ast)))))
            (entry:entry-point (parser:read-program stream source ':macro))
            (loop :for (name type value) :in (nreverse ast)
                  :do (format t "~A :: ~A~%~A~%~%~%" name type value)))
          nil)

        (coalton:coalton
         (entry:expression-entry-point (parser:read-expression stream source)))

        ;; Fall back to reading the list manually
        (t
         (let ((collected-forms (list (cst:raw first-form)))
               (dotted-context nil))
           (loop :do
             (handler-case
                 (multiple-value-bind (form presentp)
                     (parser:maybe-read-form stream source)

                   (cond
                     ((and (not presentp)
                           dotted-context)
                      (error "Invalid dotted list"))

                     ((not presentp)
                      (return-from read-coalton-toplevel-open-paren
                        (nreverse collected-forms)))

                     (dotted-context
                      (when (nth-value 1 (parser:maybe-read-form stream source))
                        (error "Invalid dotted list"))

                      (return-from read-coalton-toplevel-open-paren
                        (nreconc collected-forms (cst:raw form))))

                     (t
                      (push (cst:raw form) collected-forms))))
               (eclector.reader:invalid-context-for-consing-dot (c)
                 (when dotted-context
                   (error "Invalid dotted list"))
                 (setf dotted-context t)
                 (eclector.reader:recover c))))))))))

(named-readtables:defreadtable coalton:coalton
  (:merge :standard)
  (:macro-char #\( 'read-coalton-toplevel-open-paren)
  (:macro-char #\` (lambda (s c)
                     (let ((*coalton-reader-allowed* nil))
                       (funcall (get-macro-character #\` (named-readtables:ensure-readtable :standard)) s c))))
  (:macro-char #\, (lambda (s c)
                     (let ((*coalton-reader-allowed* t))
                       (funcall (get-macro-character #\, (named-readtables:ensure-readtable :standard)) s c)))))

(defun print-form (form)
  "Prevent truncation of a FORM that will be immediately re-read."
  (let ((*print-length* nil)
        (*print-level* nil)
        (*print-circle* t))
    (prin1-to-string form)))

(defun compile-forms (mode forms)
  "Compile FORMS as Coalton using the indicated MODE."
  (let* ((*readtable* (named-readtables:ensure-readtable 'coalton:coalton))
         (string (print-form (cons mode forms)))
         (*source* (unless (source-filename)
                     (coalton-impl/source:make-source-string string
                                                             :name (buffer-name)))))
    (with-input-from-string (stream string)
      (cl:read stream))))

(defmacro coalton:coalton-toplevel (&body forms)
  "Compile Coalton FORMS."
  (compile-forms 'coalton:coalton-toplevel forms))

(defmacro coalton:coalton-codegen (&body forms)
  "Generate code for FORMS, excluding Lisp type declarations."
  (compile-forms 'coalton:coalton-codegen forms))
(defmacro coalton:coalton-codegen-types (&body forms)
  "Generate code for FORMS, including Lisp type declarations."
  (compile-forms 'coalton:coalton-codegen-types forms))

(defmacro coalton:coalton-codegen-ast (&body forms)
  "Dump the AST for toplevel definitions occurring in FORMS to *standard-out* and return NIL."
  (compile-forms 'coalton:coalton-codegen-ast forms))

(defmacro coalton:coalton (&rest forms)
  (compile-forms 'coalton:coalton forms))

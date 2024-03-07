(defpackage #:coalton-impl/reader
  (:use
   #:cl)
  (:local-nicknames
   (#:cst #:concrete-syntax-tree)
   (#:settings #:coalton-impl/settings)
   (#:util #:coalton-impl/util)
   (#:error #:coalton-impl/error)
   (#:parser #:coalton-impl/parser)
   (#:tc #:coalton-impl/typechecker)
   (#:entry #:coalton-impl/entry)))

(in-package #:coalton-impl/reader)

(defvar *coalton-reader-allowed* t
  "Is the Coalton reader allowed to parse the current input?
Used to forbid reading while inside quasiquoted forms.")

(defvar *source-plist* nil)

(defun make-source-file ()
  (destructuring-bind (&key emacs-filename emacs-position &allow-other-keys) *source-plist*
    (if emacs-filename
        (error:make-coalton-ide-file *compile-file-truename*
                                     emacs-filename
                                     emacs-position)
        (error:make-coalton-file *compile-file-truename*))))

(defun read-coalton-toplevel-open-paren (stream char)
  (unless *coalton-reader-allowed*
    (return-from read-coalton-toplevel-open-paren
      (funcall (get-macro-character #\( (named-readtables:ensure-readtable :standard)) stream char)))
  (parser:with-reader-context stream
    (let ((first-form
            (multiple-value-bind (form presentp)
                (parser:maybe-read-form stream)
              (unless presentp
                (return-from read-coalton-toplevel-open-paren nil))
              form))
          (file (make-source-file)))
      (case (cst:raw first-form)
        (coalton:coalton-toplevel
          (multiple-value-bind (program env)
              (entry:entry-point (parser:read-program stream file :mode :toplevel-macro))
            (setf entry:*global-environment* env)
            program))
        (coalton:coalton-codegen
          (let ((settings:*coalton-skip-update* t)
                (settings:*emit-type-annotations* nil))
            (multiple-value-bind (program env)
                (entry:entry-point (parser:read-program stream file :mode :toplevel-macro))
              (declare (ignore env))
              `',program)))
        (coalton:coalton-codegen-ast
          (let ((settings:*coalton-skip-update* t)
                (settings:*emit-type-annotations* nil)
                (settings:*coalton-dump-ast* t))
            (multiple-value-bind (program env)
                (entry:entry-point (parser:read-program stream file :mode :toplevel-macro))
              (declare (ignore program env))
              nil)))
        (coalton:coalton
         (entry:expression-entry-point (parser:read-expression stream file) file))
        ;; Fall back to reading the list manually
        (t
         (let ((collected-forms (list (cst:raw first-form)))
               (dotted-context nil))
           (loop :do
             (handler-case
                 (multiple-value-bind (form presentp)
                     (parser:maybe-read-form stream)

                   (cond
                     ((and (not presentp)
                           dotted-context)
                      (error "Invalid dotted list"))

                     ((not presentp)
                      (return-from read-coalton-toplevel-open-paren
                        (nreverse collected-forms)))

                     (dotted-context
                      (when (nth-value 1 (parser:maybe-read-form stream))
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

(defmacro coalton:coalton-toplevel (&body forms)
  (let ((*readtable* (named-readtables:ensure-readtable 'coalton:coalton))
        (*print-circle* t))
    (with-input-from-string (stream (cl:format cl:nil "~S" (cons 'coalton:coalton-toplevel forms)))
      (cl:read stream))))

(defmacro coalton:coalton-codegen (&body forms)
  (let ((*readtable* (named-readtables:ensure-readtable 'coalton:coalton))
        (*print-circle* t))
    (with-input-from-string (stream (cl:format cl:nil "~S" (cons 'coalton:coalton-codegen forms)))
      (cl:read stream))))

(defmacro coalton:coalton-codegen-ast (&body forms)
  (let ((*readtable* (named-readtables:ensure-readtable 'coalton:coalton))
        (*print-circle* t))
    (with-input-from-string (stream (cl:format cl:nil "~S" (cons 'coalton:coalton-codegen-ast forms)))
      (cl:read stream))))

(defmacro coalton:coalton (&rest forms)
  (let ((*readtable* (named-readtables:ensure-readtable 'coalton:coalton))
        (*print-circle* t))
    (with-input-from-string (stream (cl:format cl:nil "~S" (cons 'coalton:coalton forms)))
      (cl:read stream))))

(in-package #:cl-user)

(defun parse-coalton (input-file)
  (with-open-file (stream input-file)
    (coalton-impl/parser:with-reader-context stream
      (coalton-impl/parser:read-program
       stream
       (coalton-impl/error:make-coalton-file :stream stream
                                             :name "file")
       :mode :file))))

(defun compile-coalton (input-file output-file)
  (let ((program (parse-coalton input-file)))
    (with-open-file (stream output-file
                            :direction :output
                            :element-type 'character
                            :if-exists :supersede)
      (coalton-impl/parser/module::generate-defpackage stream (coalton-impl/parser/toplevel::program-package-decl program))
      (coalton-impl/codegen:emit-forms stream '(named-readtables:in-readtable coalton:coalton) stream)
      (coalton-impl/entry::emit stream program))))


(compile-coalton "small.coalton" "small.lisp")




(defun compile-and-load (coalton-form)
  (uiop:with-temporary-file (:stream out-stream
                             :pathname input-file
                             :suffix "lisp"
                             :direction :output
                             :keep t)
    (prin1 coalton-form out-stream)
    (terpri out-stream)
    :close-stream
    (uiop:with-temporary-file (:pathname output-file
                               :type #+ccl (pathname-type ccl:*.fasl-pathname*)
                                     #+(not ccl) "fasl"
                               :keep t)
      (compile-file input-file :output-file output-file)
;;      (load output-file)
      (values input-file output-file))))


(compile-coalton (parsed-example) "/Users/jlbouwman/doot.lisp")

(coalton-impl/entry:entry-point (parsed-example))



  (coalton-impl/parser/module::package-decl-use
   (coalton-impl/parser/toplevel::program-package-decl (parsed-example)))


((:name "01-small-fact-fib.coal" :parse-summary (:types 0 :type-aliases 0 :structs 0 :classes 0 :instances 0 :declares 2 :defines 2 :specializations 0 :lisp-forms 0) :signatures (("FACT" . "Integer → Integer") ("FIB" . "Integer → Integer")) :codegen-text "(progn
 (declaim (sb-ext:muffle-conditions sb-kernel:redefinition-warning))
 (locally
  (declare (optimize (sb-c::type-check 0)))
  (coalton-impl/global-lexical:define-global-lexical
   small-coalton-programs/fact-fib:fact function))
 (declaim
  (ftype (function (integer) (values integer &optional))
   small-coalton-programs/fact-fib:fact))
 (locally
  (declare (optimize (sb-c::type-check 0)))
  (defun small-coalton-programs/fact-fib:fact (#1=#:g1)
    (declare (ignorable #1#))
    (the (values integer &optional)
         (let ((#2=#:g2 #1#))
           (declare (ignorable #2#)
                    (type integer #2#))
           (locally
            (declare (sb-ext:muffle-conditions sb-ext:code-deletion-note))
            (cond ((eql 0 #2#) 1)
                  (t
                   (let ((#3=#:g3
                          (small-coalton-programs/fact-fib:fact
                           (locally
                            (declare
                             (optimize (safety 0) (sb-c::type-check 0)))
                            (let ((#4=#:g4 1))
                              (declare (ignorable #4#))
                              (the (values integer &optional)
                                   (let ((coalton/math/num-defining-macros::a
                                          #1#)
                                         (coalton/math/num-defining-macros::b
                                          #4#))
                                     (values
                                      (identity
                                       (- coalton/math/num-defining-macros::a
                                          coalton/math/num-defining-macros::b))))))))))
                     (declare (ignorable #3#))
                     (locally
                      (declare (optimize (safety 0) (sb-c::type-check 0)))
                      (the (values integer &optional)
                           (let ((coalton/math/num-defining-macros::a #1#)
                                 (coalton/math/num-defining-macros::b #3#))
                             (values
                              (identity
                               (* coalton/math/num-defining-macros::a
                                  coalton/math/num-defining-macros::b)))))))))))))
  (setf small-coalton-programs/fact-fib:fact
          #'small-coalton-programs/fact-fib:fact)
  (setf (documentation 'small-coalton-programs/fact-fib:fact 'variable)
          #5=\"Compute the factorial of a given integer\")
  (setf (documentation 'small-coalton-programs/fact-fib:fact 'function) #5#))
 (locally
  (declare (optimize (sb-c::type-check 0)))
  (coalton-impl/global-lexical:define-global-lexical
   small-coalton-programs/fact-fib:fib function))
 (declaim
  (ftype (function (integer) (values integer &optional))
   small-coalton-programs/fact-fib:fib))
 (locally
  (declare (optimize (sb-c::type-check 0)))
  (defun small-coalton-programs/fact-fib:fib (#6=#:g5)
    (declare (ignorable #6#))
    (the (values integer &optional)
         (let ((#7=#:g6 #6#))
           (declare (ignorable #7#)
                    (type integer #7#))
           (locally
            (declare (sb-ext:muffle-conditions sb-ext:code-deletion-note))
            (cond ((eql 0 #7#) 0) ((eql 1 #7#) 1)
                  (t
                   (let ((#8=#:g7
                          (small-coalton-programs/fact-fib:fib
                           (let ((#9=#:g8
                                  (locally
                                   (declare
                                    (optimize (safety 0) (sb-c::type-check 0)))
                                   (let ((#10=#:g9 1))
                                     (declare (ignorable #10#))
                                     (the (values integer &optional)
                                          (let ((coalton/math/num-defining-macros::x
                                                 #10#))
                                            (values
                                             (identity
                                              coalton/math/num-defining-macros::x))))))))
                             (declare (ignorable #9#))
                             (locally
                              (declare
                               (optimize (safety 0) (sb-c::type-check 0)))
                              (the (values integer &optional)
                                   (let ((coalton/math/num-defining-macros::a
                                          #6#)
                                         (coalton/math/num-defining-macros::b
                                          #9#))
                                     (values
                                      (identity
                                       (- coalton/math/num-defining-macros::a
                                          coalton/math/num-defining-macros::b))))))))))
                     (declare (ignorable #8#))
                     (let ((#11=#:g10
                            (small-coalton-programs/fact-fib:fib
                             (locally
                              (declare
                               (optimize (safety 0) (sb-c::type-check 0)))
                              (let ((#12=#:g11 2))
                                (declare (ignorable #12#))
                                (the (values integer &optional)
                                     (let ((coalton/math/num-defining-macros::b
                                            #12#)
                                           (coalton/math/num-defining-macros::a
                                            #6#))
                                       (values
                                        (identity
                                         (- coalton/math/num-defining-macros::a
                                            coalton/math/num-defining-macros::b))))))))))
                       (declare (ignorable #11#))
                       (locally
                        (declare (optimize (safety 0) (sb-c::type-check 0)))
                        (the (values integer &optional)
                             (let ((coalton/math/num-defining-macros::b #11#)
                                   (coalton/math/num-defining-macros::a #8#))
                               (values
                                (identity
                                 (+ coalton/math/num-defining-macros::a
                                    coalton/math/num-defining-macros::b))))))))))))))
  (setf small-coalton-programs/fact-fib:fib
          #'small-coalton-programs/fact-fib:fib)
  (setf (documentation 'small-coalton-programs/fact-fib:fib 'variable)
          #13=\"Compute the nth Fibonacci number\")
  (setf (documentation 'small-coalton-programs/fact-fib:fib 'function) #13#))
 (declaim (sb-ext:unmuffle-conditions sb-kernel:redefinition-warning))
 (values))" :cost (:parse (:min-seconds 4.15d-4 :median-seconds 4.81d-4 :bytes 98208 :iterations 5) :typecheck (:min-seconds 0.006308d0 :median-seconds 0.006498d0 :bytes 3242352 :iterations 5) :codegen (:min-seconds 0.00303d0 :median-seconds 0.003151d0 :bytes 645504 :iterations 5))) (:name "02-medium-classes.coal" :parse-summary (:types 1 :type-aliases 0 :structs 0 :classes 1 :instances 3 :declares 4 :defines 5 :specializations 0 :lisp-forms 0) :signatures (("DEMO" . "∀ :B. (Num :B) (Summable :B) (Ord :B) ⇒ Void → Tuple Integer :B") ("TREE-FOLD-SUM" . "∀ :A. Summable :A ⇒ Tree :A → :A") ("TREE-FROM-LIST" . "∀ :A. Ord :A ⇒ List :A → Tree :A") ("TREE-INSERT" . "∀ :A. Ord :A ⇒ :A * Tree :A → Tree :A") ("TREE-SIZE" . "∀ :A. Tree :A → Integer")) :codegen-text "(progn
 (declaim (sb-ext:muffle-conditions sb-kernel:redefinition-warning))
 (eval-when (:compile-toplevel :load-toplevel :execute)
   (defclass coalton-benchmark/medium-classes::tree nil nil)
   (defmethod make-load-form
              (
               (coalton-benchmark/medium-classes::obj
                coalton-benchmark/medium-classes::tree)
               &optional coalton-benchmark/medium-classes::env)
     (make-load-form-saving-slots coalton-benchmark/medium-classes::obj
                                  :environment
                                  coalton-benchmark/medium-classes::env))
   (defclass coalton-benchmark/medium-classes::tree/node
             (coalton-benchmark/medium-classes::tree)
             ((coalton-benchmark/medium-classes::|_0| :type
               coalton-benchmark/medium-classes::tree :initarg
               coalton-benchmark/medium-classes::|_0|)
              (coalton-benchmark/medium-classes::|_1| :type t :initarg
               coalton-benchmark/medium-classes::|_1|)
              (coalton-benchmark/medium-classes::|_2| :type
               coalton-benchmark/medium-classes::tree :initarg
               coalton-benchmark/medium-classes::|_2|)))
   (declaim
    (ftype
     (function (coalton-benchmark/medium-classes::tree/node)
      (values coalton-benchmark/medium-classes::tree &optional))
     coalton-benchmark/medium-classes::tree/node-_0))
   (defun coalton-benchmark/medium-classes::tree/node-_0
          (coalton-impl/codegen/struct-or-class::obj)
     (slot-value coalton-impl/codegen/struct-or-class::obj
                 'coalton-benchmark/medium-classes::|_0|))
   (declaim
    (ftype
     (function (coalton-benchmark/medium-classes::tree/node)
      (values t &optional))
     coalton-benchmark/medium-classes::tree/node-_1))
   (defun coalton-benchmark/medium-classes::tree/node-_1
          (coalton-impl/codegen/struct-or-class::obj)
     (slot-value coalton-impl/codegen/struct-or-class::obj
                 'coalton-benchmark/medium-classes::|_1|))
   (declaim
    (ftype
     (function (coalton-benchmark/medium-classes::tree/node)
      (values coalton-benchmark/medium-classes::tree &optional))
     coalton-benchmark/medium-classes::tree/node-_2))
   (defun coalton-benchmark/medium-classes::tree/node-_2
          (coalton-impl/codegen/struct-or-class::obj)
     (slot-value coalton-impl/codegen/struct-or-class::obj
                 'coalton-benchmark/medium-classes::|_2|))
   (declaim
    (ftype
     (function
      (coalton-benchmark/medium-classes::tree t
       coalton-benchmark/medium-classes::tree)
      (values coalton-benchmark/medium-classes::tree/node &optional))
     coalton-benchmark/medium-classes::node))
   (defun coalton-benchmark/medium-classes::node
          (coalton-benchmark/medium-classes::|_0|
           coalton-benchmark/medium-classes::|_1|
           coalton-benchmark/medium-classes::|_2|)
     (make-instance 'coalton-benchmark/medium-classes::tree/node
                    'coalton-benchmark/medium-classes::|_0|
                    coalton-benchmark/medium-classes::|_0|
                    'coalton-benchmark/medium-classes::|_1|
                    coalton-benchmark/medium-classes::|_1|
                    'coalton-benchmark/medium-classes::|_2|
                    coalton-benchmark/medium-classes::|_2|))
   (coalton-impl/global-lexical:define-global-lexical
    coalton-benchmark/medium-classes::node function)
   (setf coalton-benchmark/medium-classes::node
           #'coalton-benchmark/medium-classes::node)
   (coalton-impl/global-lexical:define-global-lexical
    coalton-benchmark/medium-classes::tree/node-_0 function)
   (setf coalton-benchmark/medium-classes::tree/node-_0
           #'coalton-benchmark/medium-classes::tree/node-_0)
   (coalton-impl/global-lexical:define-global-lexical
    coalton-benchmark/medium-classes::tree/node-_1 function)
   (setf coalton-benchmark/medium-classes::tree/node-_1
           #'coalton-benchmark/medium-classes::tree/node-_1)
   (coalton-impl/global-lexical:define-global-lexical
    coalton-benchmark/medium-classes::tree/node-_2 function)
   (setf coalton-benchmark/medium-classes::tree/node-_2
           #'coalton-benchmark/medium-classes::tree/node-_2)
   (defmethod print-object
              (
               (coalton-impl/codegen/codegen-type-definition::self
                coalton-benchmark/medium-classes::tree/node)
               stream)
     (declare (type stream stream)
              (type coalton-benchmark/medium-classes::tree/node
               coalton-impl/codegen/codegen-type-definition::self))
     (format stream \"#.(~s\" 'coalton-benchmark/medium-classes::node)
     (cond
      ((and *print-readably* (not *read-eval*))
       (error 'print-not-readable :object
              coalton-impl/codegen/codegen-type-definition::self))
      ((and *print-readably*
            (or
             (listp
              (slot-value coalton-impl/codegen/codegen-type-definition::self
                          'coalton-benchmark/medium-classes::|_0|))
             (symbolp
              (slot-value coalton-impl/codegen/codegen-type-definition::self
                          'coalton-benchmark/medium-classes::|_0|))))
       (write-string #1=\" '\" stream)
       (prin1
        (slot-value coalton-impl/codegen/codegen-type-definition::self
                    'coalton-benchmark/medium-classes::|_0|)
        stream))
      (t (write-string #2=\" \" stream)
       (prin1
        (slot-value coalton-impl/codegen/codegen-type-definition::self
                    'coalton-benchmark/medium-classes::|_0|)
        stream)))
     (cond
      ((and *print-readably* (not *read-eval*))
       (error 'print-not-readable :object
              coalton-impl/codegen/codegen-type-definition::self))
      ((and *print-readably*
            (or
             (listp
              (slot-value coalton-impl/codegen/codegen-type-definition::self
                          'coalton-benchmark/medium-classes::|_1|))
             (symbolp
              (slot-value coalton-impl/codegen/codegen-type-definition::self
                          'coalton-benchmark/medium-classes::|_1|))))
       (write-string #1# stream)
       (prin1
        (slot-value coalton-impl/codegen/codegen-type-definition::self
                    'coalton-benchmark/medium-classes::|_1|)
        stream))
      (t (write-string #2# stream)
       (prin1
        (slot-value coalton-impl/codegen/codegen-type-definition::self
                    'coalton-benchmark/medium-classes::|_1|)
        stream)))
     (cond
      ((and *print-readably* (not *read-eval*))
       (error 'print-not-readable :object
              coalton-impl/codegen/codegen-type-definition::self))
      ((and *print-readably*
            (or
             (listp
              (slot-value coalton-impl/codegen/codegen-type-definition::self
                          'coalton-benchmark/medium-classes::|_2|))
             (symbolp
              (slot-value coalton-impl/codegen/codegen-type-definition::self
                          'coalton-benchmark/medium-classes::|_2|))))
       (write-string #1# stream)
       (prin1
        (slot-value coalton-impl/codegen/codegen-type-definition::self
                    'coalton-benchmark/medium-classes::|_2|)
        stream))
      (t (write-string #2# stream)
       (prin1
        (slot-value coalton-impl/codegen/codegen-type-definition::self
                    'coalton-benchmark/medium-classes::|_2|)
        stream)))
     (write-string \")\" stream)
     coalton-impl/codegen/codegen-type-definition::self)
   (defclass coalton-benchmark/medium-classes::tree/leaf
             (coalton-benchmark/medium-classes::tree) nil)
   (declaim
    (ftype
     (function nil
      (values coalton-benchmark/medium-classes::tree/leaf &optional))
     coalton-benchmark/medium-classes::leaf))
   (defun coalton-benchmark/medium-classes::leaf ()
     (make-instance 'coalton-benchmark/medium-classes::tree/leaf))
   (coalton-impl/global-lexical:define-global-lexical
    coalton-benchmark/medium-classes::leaf
    coalton-benchmark/medium-classes::tree/leaf)
   (setf coalton-benchmark/medium-classes::leaf
           (coalton-benchmark/medium-classes::leaf))
   (defmethod print-object
              (
               (coalton-impl/codegen/codegen-type-definition::self
                coalton-benchmark/medium-classes::tree/leaf)
               stream)
     (declare (type stream stream)
              (type coalton-benchmark/medium-classes::tree/leaf
               coalton-impl/codegen/codegen-type-definition::self))
     (format stream \"#.~s\" 'coalton-benchmark/medium-classes::leaf)
     coalton-impl/codegen/codegen-type-definition::self))
 (eval-when (:compile-toplevel :load-toplevel :execute)
   (defclass coalton-benchmark/medium-classes::class/summable nil
             ((coalton-benchmark/medium-classes::zero :type t :initarg
               coalton-benchmark/medium-classes::zero)
              (coalton-benchmark/medium-classes::combine :type function
               :initarg coalton-benchmark/medium-classes::combine)))
   (declaim
    (ftype
     (function (coalton-benchmark/medium-classes::class/summable)
      (values t &optional))
     coalton-benchmark/medium-classes::class/summable-zero))
   (defun coalton-benchmark/medium-classes::class/summable-zero
          (coalton-impl/codegen/struct-or-class::obj)
     (slot-value coalton-impl/codegen/struct-or-class::obj
                 'coalton-benchmark/medium-classes::zero))
   (declaim
    (ftype
     (function (coalton-benchmark/medium-classes::class/summable)
      (values function &optional))
     coalton-benchmark/medium-classes::class/summable-combine))
   (defun coalton-benchmark/medium-classes::class/summable-combine
          (coalton-impl/codegen/struct-or-class::obj)
     (slot-value coalton-impl/codegen/struct-or-class::obj
                 'coalton-benchmark/medium-classes::combine))
   (declaim
    (ftype
     (function (t function)
      (values coalton-benchmark/medium-classes::class/summable &optional))
     coalton-benchmark/medium-classes::class/summable))
   (defun coalton-benchmark/medium-classes::class/summable
          (coalton-benchmark/medium-classes::zero
           coalton-benchmark/medium-classes::combine)
     (make-instance 'coalton-benchmark/medium-classes::class/summable
                    'coalton-benchmark/medium-classes::zero
                    coalton-benchmark/medium-classes::zero
                    'coalton-benchmark/medium-classes::combine
                    coalton-benchmark/medium-classes::combine))
   (coalton-impl/global-lexical:define-global-lexical
    coalton-benchmark/medium-classes::class/summable function)
   (setf coalton-benchmark/medium-classes::class/summable
           #'coalton-benchmark/medium-classes::class/summable)
   (coalton-impl/global-lexical:define-global-lexical
    coalton-benchmark/medium-classes::class/summable-zero function)
   (setf coalton-benchmark/medium-classes::class/summable-zero
           #'coalton-benchmark/medium-classes::class/summable-zero)
   (coalton-impl/global-lexical:define-global-lexical
    coalton-benchmark/medium-classes::class/summable-combine function)
   (setf coalton-benchmark/medium-classes::class/summable-combine
           #'coalton-benchmark/medium-classes::class/summable-combine)
   (declaim (inline coalton-benchmark/medium-classes::zero))
   (defun coalton-benchmark/medium-classes::zero
          (coalton-impl/codegen/codegen-class::dict)
     (declare (optimize (speed 3) (safety 0)))
     (declare (ignorable coalton-impl/codegen/codegen-class::dict))
     (coalton-benchmark/medium-classes::class/summable-zero
      coalton-impl/codegen/codegen-class::dict))
   (coalton-impl/global-lexical:define-global-lexical
    coalton-benchmark/medium-classes::zero function)
   (setf coalton-benchmark/medium-classes::zero
           #'coalton-benchmark/medium-classes::zero)
   (declaim (inline coalton-benchmark/medium-classes::combine))
   (defun coalton-benchmark/medium-classes::combine
          (coalton-impl/codegen/codegen-class::dict
           coalton-benchmark/medium-classes::|_0|
           coalton-benchmark/medium-classes::|_1|)
     (declare (optimize (speed 3) (safety 0)))
     (declare
      (ignorable coalton-impl/codegen/codegen-class::dict
       coalton-benchmark/medium-classes::|_0|
       coalton-benchmark/medium-classes::|_1|))
     (funcall
      (coalton-benchmark/medium-classes::class/summable-combine
       coalton-impl/codegen/codegen-class::dict)
      coalton-benchmark/medium-classes::|_0|
      coalton-benchmark/medium-classes::|_1|))
   (coalton-impl/global-lexical:define-global-lexical
    coalton-benchmark/medium-classes::combine function)
   (setf coalton-benchmark/medium-classes::combine
           #'coalton-benchmark/medium-classes::combine))
 (locally
  (declare (optimize (sb-c::type-check 0)))
  (coalton-impl/global-lexical:define-global-lexical
   coalton-benchmark/medium-classes::|INSTANCE/FUNCTOR TREE-COALTON/CLASSES:MAP|
   function))
 (declaim
  (ftype
   (function (function coalton-benchmark/medium-classes::tree)
    (values coalton-benchmark/medium-classes::tree &optional))
   coalton-benchmark/medium-classes::|INSTANCE/FUNCTOR TREE-COALTON/CLASSES:MAP|))
 (locally
  (declare (optimize (sb-c::type-check 0)))
  (defun coalton-benchmark/medium-classes::|INSTANCE/FUNCTOR TREE-COALTON/CLASSES:MAP|
         (#3=#:g1 #4=#:g2)
    (declare (ignorable #3# #4#))
    (the (values coalton-benchmark/medium-classes::tree &optional)
         (let ((#5=#:g3 #4#))
           (declare (ignorable #5#)
                    (type coalton-benchmark/medium-classes::tree #5#))
           (locally
            (declare (sb-ext:muffle-conditions sb-ext:code-deletion-note))
            (cond
             ((and (typep #5# 'coalton-benchmark/medium-classes::tree/leaf))
              coalton-benchmark/medium-classes::leaf)
             ((and (typep #5# 'coalton-benchmark/medium-classes::tree/node) t t
                   t)
              (let ((#6=#:g4
                     (coalton-benchmark/medium-classes::tree/node-_0 #5#))
                    (#7=#:g5
                     (coalton-benchmark/medium-classes::tree/node-_1 #5#))
                    (#8=#:g6
                     (coalton-benchmark/medium-classes::tree/node-_2 #5#)))
                (declare (ignorable #6# #7# #8#)
                         (type coalton-benchmark/medium-classes::tree #6#)
                         (type t #7#)
                         (type coalton-benchmark/medium-classes::tree #8#))
                (coalton-benchmark/medium-classes::node
                 (coalton-benchmark/medium-classes::|INSTANCE/FUNCTOR TREE-COALTON/CLASSES:MAP|
                  #3# #6#)
                 (funcall #3# #7#)
                 (coalton-benchmark/medium-classes::|INSTANCE/FUNCTOR TREE-COALTON/CLASSES:MAP|
                  #3# #8#))))
             (t (error #9=\"Pattern match not exhaustive error.\")))))))
  (setf coalton-benchmark/medium-classes::|INSTANCE/FUNCTOR TREE-COALTON/CLASSES:MAP|
          #'coalton-benchmark/medium-classes::|INSTANCE/FUNCTOR TREE-COALTON/CLASSES:MAP|))
 (locally
  (declare (optimize (sb-c::type-check 0)))
  (coalton-impl/global-lexical:define-global-lexical
   coalton-benchmark/medium-classes::|INSTANCE/FUNCTOR TREE|
   coalton/classes::class/functor))
 (locally
  (declare (optimize (sb-c::type-check 0)))
  (setf coalton-benchmark/medium-classes::|INSTANCE/FUNCTOR TREE|
          (coalton/classes::class/functor
           coalton-benchmark/medium-classes::|INSTANCE/FUNCTOR TREE-COALTON/CLASSES:MAP|)))
 (locally
  (declare (optimize (sb-c::type-check 0)))
  (coalton-impl/global-lexical:define-global-lexical
   coalton-benchmark/medium-classes::tree-fold-sum function))
 (declaim
  (ftype
   (function
    (coalton-benchmark/medium-classes::class/summable
     coalton-benchmark/medium-classes::tree)
    (values t &optional))
   coalton-benchmark/medium-classes::tree-fold-sum))
 (locally
  (declare (optimize (sb-c::type-check 0)))
  (defun coalton-benchmark/medium-classes::tree-fold-sum (#10=#:g7 #11=#:g8)
    (declare (ignorable #10# #11#))
    (the (values t &optional)
         (let ((#12=#:g9 #11#))
           (declare (ignorable #12#)
                    (type coalton-benchmark/medium-classes::tree #12#))
           (locally
            (declare (sb-ext:muffle-conditions sb-ext:code-deletion-note))
            (cond
             ((and (typep #12# 'coalton-benchmark/medium-classes::tree/leaf))
              (coalton-benchmark/medium-classes::zero #10#))
             ((and (typep #12# 'coalton-benchmark/medium-classes::tree/node) t
                   t t)
              (let ((#13=#:g10
                     (coalton-benchmark/medium-classes::tree/node-_0 #12#))
                    (#14=#:g11
                     (coalton-benchmark/medium-classes::tree/node-_1 #12#))
                    (#15=#:g12
                     (coalton-benchmark/medium-classes::tree/node-_2 #12#)))
                (declare (ignorable #13# #14# #15#)
                         (type coalton-benchmark/medium-classes::tree #13#)
                         (type t #14#)
                         (type coalton-benchmark/medium-classes::tree #15#))
                (coalton-benchmark/medium-classes::combine #10#
                 (coalton-benchmark/medium-classes::combine #10#
                  (coalton-benchmark/medium-classes::tree-fold-sum #10# #13#)
                  #14#)
                 (coalton-benchmark/medium-classes::tree-fold-sum #10# #15#))))
             (t (error #9#)))))))
  (setf coalton-benchmark/medium-classes::tree-fold-sum
          #'coalton-benchmark/medium-classes::tree-fold-sum))
 (locally
  (declare (optimize (sb-c::type-check 0)))
  (coalton-impl/global-lexical:define-global-lexical
   coalton-benchmark/medium-classes::tree-insert function))
 (declaim
  (ftype
   (function
    (coalton/classes::class/ord t coalton-benchmark/medium-classes::tree)
    (values coalton-benchmark/medium-classes::tree &optional))
   coalton-benchmark/medium-classes::tree-insert))
 (locally
  (declare (optimize (sb-c::type-check 0)))
  (defun coalton-benchmark/medium-classes::tree-insert
         (#16=#:g13 #17=#:g14 #18=#:g15)
    (declare (ignorable #16# #17# #18#))
    (the (values coalton-benchmark/medium-classes::tree &optional)
         (let ((#19=#:g16 #18#))
           (declare (ignorable #19#)
                    (type coalton-benchmark/medium-classes::tree #19#))
           (locally
            (declare (sb-ext:muffle-conditions sb-ext:code-deletion-note))
            (cond
             ((and (typep #19# 'coalton-benchmark/medium-classes::tree/leaf))
              (coalton-benchmark/medium-classes::node
               coalton-benchmark/medium-classes::leaf #17#
               coalton-benchmark/medium-classes::leaf))
             ((and (typep #19# 'coalton-benchmark/medium-classes::tree/node) t
                   t t)
              (let ((#20=#:g17
                     (coalton-benchmark/medium-classes::tree/node-_0 #19#))
                    (#21=#:g18
                     (coalton-benchmark/medium-classes::tree/node-_1 #19#))
                    (#22=#:g19
                     (coalton-benchmark/medium-classes::tree/node-_2 #19#)))
                (declare (ignorable #20# #21# #22#)
                         (type coalton-benchmark/medium-classes::tree #20#)
                         (type t #21#)
                         (type coalton-benchmark/medium-classes::tree #22#))
                (let ((#23=#:g20 (coalton/classes:<=> #16# #17# #21#)))
                  (declare (ignorable #23#)
                           (type
                            (member coalton/classes::ordering/gt
                                    coalton/classes::ordering/eq
                                    coalton/classes::ordering/lt)
                            #23#))
                  (locally
                   (declare
                    (sb-ext:muffle-conditions sb-ext:code-deletion-note))
                   (case #23#
                     ((coalton/classes::ordering/lt)
                      (coalton-benchmark/medium-classes::node
                       (coalton-benchmark/medium-classes::tree-insert #16# #17#
                        #20#)
                       #21# #22#))
                     ((coalton/classes::ordering/gt)
                      (coalton-benchmark/medium-classes::node #20# #21#
                       (coalton-benchmark/medium-classes::tree-insert #16# #17#
                        #22#)))
                     ((coalton/classes::ordering/eq)
                      (coalton-benchmark/medium-classes::node #20# #21# #22#))
                     (otherwise (error #9#)))))))
             (t (error #9#)))))))
  (setf coalton-benchmark/medium-classes::tree-insert
          #'coalton-benchmark/medium-classes::tree-insert))
 (locally
  (declare (optimize (sb-c::type-check 0)))
  (coalton-impl/global-lexical:define-global-lexical
   coalton-benchmark/medium-classes::tree-from-list function))
 (declaim
  (ftype
   (function (coalton/classes::class/ord list)
    (values coalton-benchmark/medium-classes::tree &optional))
   coalton-benchmark/medium-classes::tree-from-list))
 (locally
  (declare (optimize (sb-c::type-check 0)))
  (defun coalton-benchmark/medium-classes::tree-from-list (#24=#:g21 #25=#:g22)
    (declare (ignorable #24# #25#))
    (the (values coalton-benchmark/medium-classes::tree &optional)
         (let ((#26=#:g23 #25#))
           (declare (ignorable #26#)
                    (type list #26#))
           (locally
            (declare (sb-ext:muffle-conditions sb-ext:code-deletion-note))
            (cond ((null #26#) coalton-benchmark/medium-classes::leaf)
                  ((and (consp #26#) t t)
                   (let ((#27=#:g24 (car #26#)) (#28=#:g25 (cdr #26#)))
                     (declare (ignorable #27# #28#)
                              (type t #27#)
                              (type list #28#))
                     (coalton-benchmark/medium-classes::tree-insert #24# #27#
                      (coalton-benchmark/medium-classes::tree-from-list #24#
                       #28#))))
                  (t (error #9#)))))))
  (setf coalton-benchmark/medium-classes::tree-from-list
          #'coalton-benchmark/medium-classes::tree-from-list))
 (locally
  (declare (optimize (sb-c::type-check 0)))
  (coalton-impl/global-lexical:define-global-lexical
   coalton-benchmark/medium-classes::tree-size function))
 (declaim
  (ftype
   (function (coalton-benchmark/medium-classes::tree)
    (values integer &optional))
   coalton-benchmark/medium-classes::tree-size))
 (locally
  (declare (optimize (sb-c::type-check 0)))
  (defun coalton-benchmark/medium-classes::tree-size (#29=#:g26)
    (declare (ignorable #29#))
    (the (values integer &optional)
         (let ((#30=#:g27 #29#))
           (declare (ignorable #30#)
                    (type coalton-benchmark/medium-classes::tree #30#))
           (locally
            (declare (sb-ext:muffle-conditions sb-ext:code-deletion-note))
            (cond
             ((and (typep #30# 'coalton-benchmark/medium-classes::tree/leaf))
              0)
             ((and (typep #30# 'coalton-benchmark/medium-classes::tree/node) t
                   t t)
              (let ((#31=#:g28
                     (coalton-benchmark/medium-classes::tree/node-_0 #30#))
                    (#32=#:g29
                     (coalton-benchmark/medium-classes::tree/node-_2 #30#)))
                (declare (ignorable #31# #32#)
                         (type coalton-benchmark/medium-classes::tree #31#)
                         (type coalton-benchmark/medium-classes::tree #32#))
                (let ((#33=#:g30
                       (let ((#34=#:g31
                              (coalton-benchmark/medium-classes::tree-size
                               #31#)))
                         (declare (ignorable #34#))
                         (let ((#35=#:g32
                                (coalton-benchmark/medium-classes::tree-size
                                 #32#)))
                           (declare (ignorable #35#))
                           (locally
                            (declare
                             (optimize (safety 0) (sb-c::type-check 0)))
                            (the (values integer &optional)
                                 (let ((coalton/math/num-defining-macros::b
                                        #35#)
                                       (coalton/math/num-defining-macros::a
                                        #34#))
                                   (values
                                    (identity
                                     (+ coalton/math/num-defining-macros::a
                                        coalton/math/num-defining-macros::b))))))))))
                  (declare (ignorable #33#))
                  (locally
                   (declare (optimize (safety 0) (sb-c::type-check 0)))
                   (let ((#36=#:g33 1))
                     (declare (ignorable #36#))
                     (the (values integer &optional)
                          (let ((coalton/math/num-defining-macros::a #36#)
                                (coalton/math/num-defining-macros::b #33#))
                            (values
                             (identity
                              (+ coalton/math/num-defining-macros::a
                                 coalton/math/num-defining-macros::b))))))))))
             (t (error #9#)))))))
  (setf coalton-benchmark/medium-classes::tree-size
          #'coalton-benchmark/medium-classes::tree-size))
 (locally
  (declare (optimize (sb-c::type-check 0)))
  (coalton-impl/global-lexical:define-global-lexical
   coalton-benchmark/medium-classes::demo function))
 (declaim
  (ftype
   (function
    (coalton/classes::class/num
     coalton-benchmark/medium-classes::class/summable
     coalton/classes::class/ord)
    (values coalton/classes:tuple &optional))
   coalton-benchmark/medium-classes::demo))
 (locally
  (declare (optimize (sb-c::type-check 0)))
  (defun coalton-benchmark/medium-classes::demo (#37=#:g34 #38=#:g35 #39=#:g36)
    (declare (ignorable #37# #38# #39#))
    (the (values coalton/classes:tuple &optional)
         (let ((#40=#:g37
                (coalton-benchmark/medium-classes::tree-from-list #39#
                 (coalton:cons (coalton/classes:fromint #37# 5)
                               (coalton:cons (coalton/classes:fromint #37# 3)
                                             (coalton:cons
                                              (coalton/classes:fromint #37# 8)
                                              (coalton:cons
                                               (coalton/classes:fromint #37# 1)
                                               (coalton:cons
                                                (coalton/classes:fromint #37#
                                                                         4)
                                                (coalton:cons
                                                 (coalton/classes:fromint #37#
                                                                          7)
                                                 (coalton:cons
                                                  (coalton/classes:fromint #37#
                                                                           2)
                                                  'nil))))))))))
           (declare (ignorable #40#))
           (coalton/classes:tuple
            (coalton-benchmark/medium-classes::tree-size #40#)
            (coalton-benchmark/medium-classes::tree-fold-sum #38#
             (coalton-benchmark/medium-classes::|INSTANCE/FUNCTOR TREE-COALTON/CLASSES:MAP|
              (lambda (#41=#:g38)
                (declare (ignorable #41#)
                         (type t #41#)
                         (values t &optional))
                (the (values t &optional) (coalton/classes:* #37# #41# #41#)))
              #40#))))))
  (setf coalton-benchmark/medium-classes::demo
          #'coalton-benchmark/medium-classes::demo))
 (locally
  (declare (optimize (sb-c::type-check 0)))
  (coalton-impl/global-lexical:define-global-lexical
   coalton-benchmark/medium-classes::|INSTANCE/COALTON/TYPES:RUNTIMEREPR (TREE :\\|0\\|)-COALTON/TYPES:RUNTIME-REPR|
   function))
 (declaim
  (ftype
   (function ((member coalton/types::proxy/proxy))
    (values (or symbol list) &optional))
   coalton-benchmark/medium-classes::|INSTANCE/COALTON/TYPES:RUNTIMEREPR (TREE :\\|0\\|)-COALTON/TYPES:RUNTIME-REPR|))
 (declaim
  (inline
   coalton-benchmark/medium-classes::|INSTANCE/COALTON/TYPES:RUNTIMEREPR (TREE :\\|0\\|)-COALTON/TYPES:RUNTIME-REPR|))
 (locally
  (declare (optimize (sb-c::type-check 0)))
  (defun coalton-benchmark/medium-classes::|INSTANCE/COALTON/TYPES:RUNTIMEREPR (TREE :\\|0\\|)-COALTON/TYPES:RUNTIME-REPR|
         (#42=#:g39)
    (declare (ignorable #42#)
             (notinline
              coalton-benchmark/medium-classes::|INSTANCE/COALTON/TYPES:RUNTIMEREPR (TREE :\\|0\\|)-COALTON/TYPES:RUNTIME-REPR|))
    (the (values (or symbol list) &optional)
         (locally
          (declare (optimize (sb-c::type-check 1)))
          (the (values (or symbol list) &optional)
               (progn (values 'coalton-benchmark/medium-classes::tree))))))
  (setf coalton-benchmark/medium-classes::|INSTANCE/COALTON/TYPES:RUNTIMEREPR (TREE :\\|0\\|)-COALTON/TYPES:RUNTIME-REPR|
          #'coalton-benchmark/medium-classes::|INSTANCE/COALTON/TYPES:RUNTIMEREPR (TREE :\\|0\\|)-COALTON/TYPES:RUNTIME-REPR|))
 (locally
  (declare (optimize (sb-c::type-check 0)))
  (coalton-impl/global-lexical:define-global-lexical
   coalton-benchmark/medium-classes::|INSTANCE/COALTON/TYPES:RUNTIMEREPR (TREE :\\|0\\|)|
   coalton/types::class/runtimerepr))
 (locally
  (declare (optimize (sb-c::type-check 0)))
  (setf coalton-benchmark/medium-classes::|INSTANCE/COALTON/TYPES:RUNTIMEREPR (TREE :\\|0\\|)|
          (coalton/types::class/runtimerepr
           coalton-benchmark/medium-classes::|INSTANCE/COALTON/TYPES:RUNTIMEREPR (TREE :\\|0\\|)-COALTON/TYPES:RUNTIME-REPR|)))
 (locally
  (declare (optimize (sb-c::type-check 0)))
  (coalton-impl/global-lexical:define-global-lexical
   coalton-benchmark/medium-classes::|INSTANCE/SUMMABLE INTEGER-COALTON-BENCHMARK/MEDIUM-CLASSES::ZERO|
   integer))
 (locally
  (declare (optimize (sb-c::type-check 0)))
  (setf coalton-benchmark/medium-classes::|INSTANCE/SUMMABLE INTEGER-COALTON-BENCHMARK/MEDIUM-CLASSES::ZERO|
          0))
 (locally
  (declare (optimize (sb-c::type-check 0)))
  (coalton-impl/global-lexical:define-global-lexical
   coalton-benchmark/medium-classes::|INSTANCE/SUMMABLE INTEGER-COALTON-BENCHMARK/MEDIUM-CLASSES::COMBINE|
   function))
 (declaim
  (ftype (function (integer integer) (values integer &optional))
   coalton-benchmark/medium-classes::|INSTANCE/SUMMABLE INTEGER-COALTON-BENCHMARK/MEDIUM-CLASSES::COMBINE|))
 (locally
  (declare (optimize (sb-c::type-check 0)))
  (defun coalton-benchmark/medium-classes::|INSTANCE/SUMMABLE INTEGER-COALTON-BENCHMARK/MEDIUM-CLASSES::COMBINE|
         (#43=#:g40 #44=#:g41)
    (declare (ignorable #43# #44#))
    (the (values integer &optional)
         (locally
          (declare (optimize (safety 0) (sb-c::type-check 0)))
          (the (values integer &optional)
               (let ((coalton/math/num-defining-macros::a #43#)
                     (coalton/math/num-defining-macros::b #44#))
                 (values
                  (identity
                   (+ coalton/math/num-defining-macros::a
                      coalton/math/num-defining-macros::b))))))))
  (setf coalton-benchmark/medium-classes::|INSTANCE/SUMMABLE INTEGER-COALTON-BENCHMARK/MEDIUM-CLASSES::COMBINE|
          #'coalton-benchmark/medium-classes::|INSTANCE/SUMMABLE INTEGER-COALTON-BENCHMARK/MEDIUM-CLASSES::COMBINE|))
 (locally
  (declare (optimize (sb-c::type-check 0)))
  (coalton-impl/global-lexical:define-global-lexical
   coalton-benchmark/medium-classes::|INSTANCE/SUMMABLE INTEGER|
   coalton-benchmark/medium-classes::class/summable))
 (locally
  (declare (optimize (sb-c::type-check 0)))
  (setf coalton-benchmark/medium-classes::|INSTANCE/SUMMABLE INTEGER|
          (coalton-benchmark/medium-classes::class/summable
           coalton-benchmark/medium-classes::|INSTANCE/SUMMABLE INTEGER-COALTON-BENCHMARK/MEDIUM-CLASSES::ZERO|
           coalton-benchmark/medium-classes::|INSTANCE/SUMMABLE INTEGER-COALTON-BENCHMARK/MEDIUM-CLASSES::COMBINE|)))
 (locally
  (declare (optimize (sb-c::type-check 0)))
  (coalton-impl/global-lexical:define-global-lexical
   coalton-benchmark/medium-classes::|INSTANCE/SUMMABLE STRING-COALTON-BENCHMARK/MEDIUM-CLASSES::ZERO|
   string))
 (locally
  (declare (optimize (sb-c::type-check 0)))
  (setf coalton-benchmark/medium-classes::|INSTANCE/SUMMABLE STRING-COALTON-BENCHMARK/MEDIUM-CLASSES::ZERO|
          \"\"))
 (locally
  (declare (optimize (sb-c::type-check 0)))
  (coalton-impl/global-lexical:define-global-lexical
   coalton-benchmark/medium-classes::|INSTANCE/SUMMABLE STRING-COALTON-BENCHMARK/MEDIUM-CLASSES::COMBINE|
   function))
 (declaim
  (ftype (function (string string) (values string &optional))
   coalton-benchmark/medium-classes::|INSTANCE/SUMMABLE STRING-COALTON-BENCHMARK/MEDIUM-CLASSES::COMBINE|))
 (locally
  (declare (optimize (sb-c::type-check 0)))
  (defun coalton-benchmark/medium-classes::|INSTANCE/SUMMABLE STRING-COALTON-BENCHMARK/MEDIUM-CLASSES::COMBINE|
         (#45=#:g42 #46=#:g43)
    (declare (ignorable #45# #46#))
    (the (values string &optional)
         (coalton/string::|INSTANCE/SEMIGROUP STRING-COALTON/CLASSES:<>| #45#
                                                                         #46#)))
  (setf coalton-benchmark/medium-classes::|INSTANCE/SUMMABLE STRING-COALTON-BENCHMARK/MEDIUM-CLASSES::COMBINE|
          #'coalton-benchmark/medium-classes::|INSTANCE/SUMMABLE STRING-COALTON-BENCHMARK/MEDIUM-CLASSES::COMBINE|))
 (locally
  (declare (optimize (sb-c::type-check 0)))
  (coalton-impl/global-lexical:define-global-lexical
   coalton-benchmark/medium-classes::|INSTANCE/SUMMABLE STRING|
   coalton-benchmark/medium-classes::class/summable))
 (locally
  (declare (optimize (sb-c::type-check 0)))
  (setf coalton-benchmark/medium-classes::|INSTANCE/SUMMABLE STRING|
          (coalton-benchmark/medium-classes::class/summable
           coalton-benchmark/medium-classes::|INSTANCE/SUMMABLE STRING-COALTON-BENCHMARK/MEDIUM-CLASSES::ZERO|
           coalton-benchmark/medium-classes::|INSTANCE/SUMMABLE STRING-COALTON-BENCHMARK/MEDIUM-CLASSES::COMBINE|)))
 (declaim (sb-ext:unmuffle-conditions sb-kernel:redefinition-warning))
 (values))" :cost (:parse (:min-seconds 0.001734d0 :median-seconds 0.001781d0 :bytes 357792 :iterations 5) :typecheck (:min-seconds 0.035834d0 :median-seconds 0.036665d0 :bytes 16637872 :iterations 5) :codegen (:min-seconds 0.008781d0 :median-seconds 0.008992d0 :bytes 1335600 :iterations 5))) (:name "03-large-inference.coal" :parse-summary (:types 0 :type-aliases 0 :structs 0 :classes 0 :instances 0 :declares 0 :defines 26 :specializations 0 :lisp-forms 0) :signatures (("Q-ALL" . "∀ :A. (:A → Boolean) * List :A → Boolean") ("Q-ANY" . "∀ :A. (:A → Boolean) * List :A → Boolean") ("Q-APP" . "∀ :A. List :A * List :A → List :A") ("Q-APPLY-N" . "∀ :A :B. Num :A ⇒ :A * (:B → :B) * :B → :B") ("Q-COMPOSE" . "∀ :A :B :C. (:A → :B) * (:C → :A) → :C → :B") ("Q-CONCAT-MAP" . "∀ :A :B. (:A → List :B) * List :A → List :B") ("Q-COUNT" . "∀ :A :B. Num :B ⇒ (:A → Boolean) * List :A → :B") ("Q-DROP" . "∀ :A :B. Num :A ⇒ :A * List :B → List :B") ("Q-ENUMERATE" . "∀ :B :A. Num :A ⇒ List :B → List (Tuple :A :B)") ("Q-ENUMERATE-ONTO" . "∀ :A :B. Num :A ⇒ :A * List :B → List (Tuple :A :B)") ("Q-FILTER" . "∀ :A. (:A → Boolean) * List :A → List :A") ("Q-FOLDL" . "∀ :A :B. (:A * :B → :A) * :A * List :B → :A") ("Q-LEN" . "∀ :A :B. Num :B ⇒ List :A → :B") ("Q-MAP" . "∀ :A :B. (:A → :B) * List :A → List :B") ("Q-MAXIMUM-ONTO" . "∀ :A. Ord :A ⇒ :A * List :A → :A") ("Q-PIPELINE" . "∀ :B :A. (Remainder :B) (Num :A) (Ord :B) ⇒ :B → Tuple :B (Tuple :A :B)") ("Q-PRODUCT" . "∀ :A. Num :A ⇒ List :A → :A") ("Q-RANGE-DOWN" . "∀ :A. Num :A ⇒ :A → List :A") ("Q-REPLICATE" . "∀ :A :B. Num :A ⇒ :A * :B → List :B") ("Q-REV" . "∀ :A. List :A → List :A") ("Q-REV-ONTO" . "∀ :A. List :A * List :A → List :A") ("Q-SINGLE" . "∀ :A. :A → List :A") ("Q-SUM" . "∀ :A. Num :A ⇒ List :A → :A") ("Q-TAKE" . "∀ :A :B. Num :A ⇒ :A * List :B → List :B") ("Q-TWICE" . "∀ :A. (:A → :A) → :A → :A") ("Q-ZIP-WITH" . "∀ :A :B :C. (:A * :B → :C) * List :A * List :B → List :C")) :codegen-text "(progn
 (declaim (sb-ext:muffle-conditions sb-kernel:redefinition-warning))
 (locally
  (declare (optimize (sb-c::type-check 0)))
  (coalton-impl/global-lexical:define-global-lexical
   coalton-benchmark/large-inference::q-len function))
 (declaim
  (ftype (function (coalton/classes::class/num list) (values t &optional))
   coalton-benchmark/large-inference::q-len))
 (locally
  (declare (optimize (sb-c::type-check 0)))
  (defun coalton-benchmark/large-inference::q-len (#1=#:g1 #2=#:g2)
    (declare (ignorable #1# #2#))
    (the (values t &optional)
         (let ((#3=#:g3 #2#))
           (declare (ignorable #3#)
                    (type list #3#))
           (locally
            (declare (sb-ext:muffle-conditions sb-ext:code-deletion-note))
            (cond ((null #3#) (coalton/classes:fromint #1# 0))
                  ((and (consp #3#) t t)
                   (let ((#4=#:g4 (cdr #3#)))
                     (declare (ignorable #4#)
                              (type list #4#))
                     (coalton/classes:+ #1# (coalton/classes:fromint #1# 1)
                                        (coalton-benchmark/large-inference::q-len
                                         #1# #4#))))
                  (t (error #5=\"Pattern match not exhaustive error.\")))))))
  (setf coalton-benchmark/large-inference::q-len
          #'coalton-benchmark/large-inference::q-len))
 (locally
  (declare (optimize (sb-c::type-check 0)))
  (coalton-impl/global-lexical:define-global-lexical
   coalton-benchmark/large-inference::q-map function))
 (declaim
  (ftype (function (function list) (values list &optional))
   coalton-benchmark/large-inference::q-map))
 (locally
  (declare (optimize (sb-c::type-check 0)))
  (defun coalton-benchmark/large-inference::q-map (#6=#:g5 #7=#:g6)
    (declare (ignorable #6# #7#))
    (the (values list &optional)
         (let ((#8=#:g7 #7#))
           (declare (ignorable #8#)
                    (type list #8#))
           (locally
            (declare (sb-ext:muffle-conditions sb-ext:code-deletion-note))
            (cond ((null #8#) 'nil)
                  ((and (consp #8#) t t)
                   (let ((#9=#:g8 (car #8#)) (#10=#:g9 (cdr #8#)))
                     (declare (ignorable #9# #10#)
                              (type t #9#)
                              (type list #10#))
                     (coalton:cons (funcall #6# #9#)
                                   (coalton-benchmark/large-inference::q-map
                                    #6# #10#))))
                  (t (error #5#)))))))
  (setf coalton-benchmark/large-inference::q-map
          #'coalton-benchmark/large-inference::q-map))
 (locally
  (declare (optimize (sb-c::type-check 0)))
  (coalton-impl/global-lexical:define-global-lexical
   coalton-benchmark/large-inference::q-filter function))
 (declaim
  (ftype (function (function list) (values list &optional))
   coalton-benchmark/large-inference::q-filter))
 (locally
  (declare (optimize (sb-c::type-check 0)))
  (defun coalton-benchmark/large-inference::q-filter (#11=#:g10 #12=#:g11)
    (declare (ignorable #11# #12#))
    (the (values list &optional)
         (let ((#13=#:g12 #12#))
           (declare (ignorable #13#)
                    (type list #13#))
           (locally
            (declare (sb-ext:muffle-conditions sb-ext:code-deletion-note))
            (cond ((null #13#) 'nil)
                  ((and (consp #13#) t t)
                   (let ((#14=#:g13 (car #13#)) (#15=#:g14 (cdr #13#)))
                     (declare (ignorable #14# #15#)
                              (type t #14#)
                              (type list #15#))
                     (if (funcall #11# #14#)
                         (coalton:cons #14#
                                       (coalton-benchmark/large-inference::q-filter
                                        #11# #15#))
                         (coalton-benchmark/large-inference::q-filter #11#
                          #15#))))
                  (t (error #5#)))))))
  (setf coalton-benchmark/large-inference::q-filter
          #'coalton-benchmark/large-inference::q-filter))
 (locally
  (declare (optimize (sb-c::type-check 0)))
  (coalton-impl/global-lexical:define-global-lexical
   coalton-benchmark/large-inference::q-foldl function))
 (declaim
  (ftype (function (function t list) (values t &optional))
   coalton-benchmark/large-inference::q-foldl))
 (locally
  (declare (optimize (sb-c::type-check 0)))
  (defun coalton-benchmark/large-inference::q-foldl
         (#16=#:g15 #17=#:g16 #18=#:g17)
    (declare (ignorable #16# #17# #18#))
    (the (values t &optional)
         (let ((#19=#:g18 #18#))
           (declare (ignorable #19#)
                    (type list #19#))
           (locally
            (declare (sb-ext:muffle-conditions sb-ext:code-deletion-note))
            (cond ((null #19#) #17#)
                  ((and (consp #19#) t t)
                   (let ((#20=#:g19 (car #19#)) (#21=#:g20 (cdr #19#)))
                     (declare (ignorable #20# #21#)
                              (type t #20#)
                              (type list #21#))
                     (coalton-benchmark/large-inference::q-foldl #16#
                      (funcall #16# #17# #20#) #21#)))
                  (t (error #5#)))))))
  (setf coalton-benchmark/large-inference::q-foldl
          #'coalton-benchmark/large-inference::q-foldl))
 (locally
  (declare (optimize (sb-c::type-check 0)))
  (coalton-impl/global-lexical:define-global-lexical
   coalton-benchmark/large-inference::q-sum function))
 (declaim
  (ftype (function (coalton/classes::class/num list) (values t &optional))
   coalton-benchmark/large-inference::q-sum))
 (locally
  (declare (optimize (sb-c::type-check 0)))
  (defun coalton-benchmark/large-inference::q-sum (#22=#:g21 #23=#:g22)
    (declare (ignorable #22# #23#))
    (the (values t &optional)
         (coalton-benchmark/large-inference::q-foldl
          (lambda (#24=#:g23 #25=#:g24)
            (declare (ignorable #24# #25#)
                     (type t #24#)
                     (type t #25#)
                     (values t &optional))
            (the (values t &optional) (coalton/classes:+ #22# #24# #25#)))
          (coalton/classes:fromint #22# 0) #23#)))
  (setf coalton-benchmark/large-inference::q-sum
          #'coalton-benchmark/large-inference::q-sum))
 (locally
  (declare (optimize (sb-c::type-check 0)))
  (coalton-impl/global-lexical:define-global-lexical
   coalton-benchmark/large-inference::q-maximum-onto function))
 (declaim
  (ftype (function (coalton/classes::class/ord t list) (values t &optional))
   coalton-benchmark/large-inference::q-maximum-onto))
 (locally
  (declare (optimize (sb-c::type-check 0)))
  (defun coalton-benchmark/large-inference::q-maximum-onto
         (#26=#:g25 #27=#:g26 #28=#:g27)
    (declare (ignorable #26# #27# #28#))
    (the (values t &optional)
         (let ((#29=#:g28 #28#))
           (declare (ignorable #29#)
                    (type list #29#))
           (locally
            (declare (sb-ext:muffle-conditions sb-ext:code-deletion-note))
            (cond ((null #29#) #27#)
                  ((and (consp #29#) t t)
                   (let ((#30=#:g29 (car #29#)) (#31=#:g30 (cdr #29#)))
                     (declare (ignorable #30# #31#)
                              (type t #30#)
                              (type list #31#))
                     (coalton-benchmark/large-inference::q-maximum-onto #26#
                      (if (coalton/classes:> #26# #30# #27#)
                          #30#
                          #27#)
                      #31#)))
                  (t (error #5#)))))))
  (setf coalton-benchmark/large-inference::q-maximum-onto
          #'coalton-benchmark/large-inference::q-maximum-onto))
 (locally
  (declare (optimize (sb-c::type-check 0)))
  (coalton-impl/global-lexical:define-global-lexical
   coalton-benchmark/large-inference::q-single function))
 (declaim
  (ftype (function (t) (values list &optional))
   coalton-benchmark/large-inference::q-single))
 (locally
  (declare (optimize (sb-c::type-check 0)))
  (defun coalton-benchmark/large-inference::q-single (#32=#:g31)
    (declare (ignorable #32#))
    (the (values list &optional) (coalton:cons #32# 'nil)))
  (setf coalton-benchmark/large-inference::q-single
          #'coalton-benchmark/large-inference::q-single))
 (locally
  (declare (optimize (sb-c::type-check 0)))
  (coalton-impl/global-lexical:define-global-lexical
   coalton-benchmark/large-inference::q-range-down function))
 (declaim
  (ftype (function (coalton/classes::class/num t) (values list &optional))
   coalton-benchmark/large-inference::q-range-down))
 (locally
  (declare (optimize (sb-c::type-check 0)))
  (defun coalton-benchmark/large-inference::q-range-down (#33=#:g32 #34=#:g33)
    (declare (ignorable #33# #34#))
    (the (values list &optional)
         (if (coalton/classes:== (coalton/classes::class/num-super-0 #33#) #34#
                                 (coalton/classes:fromint #33# 0))
             (coalton-benchmark/large-inference::q-single
              (coalton/classes:fromint #33# 0))
             (coalton:cons #34#
                           (coalton-benchmark/large-inference::q-range-down
                            #33#
                            (coalton/classes:- #33# #34#
                                               (coalton/classes:fromint #33#
                                                                        1)))))))
  (setf coalton-benchmark/large-inference::q-range-down
          #'coalton-benchmark/large-inference::q-range-down))
 (locally
  (declare (optimize (sb-c::type-check 0)))
  (coalton-impl/global-lexical:define-global-lexical
   coalton-benchmark/large-inference::q-enumerate-onto function))
 (declaim
  (ftype (function (coalton/classes::class/num t list) (values list &optional))
   coalton-benchmark/large-inference::q-enumerate-onto))
 (locally
  (declare (optimize (sb-c::type-check 0)))
  (defun coalton-benchmark/large-inference::q-enumerate-onto
         (#35=#:g34 #36=#:g35 #37=#:g36)
    (declare (ignorable #35# #36# #37#))
    (the (values list &optional)
         (let ((#38=#:g37 #37#))
           (declare (ignorable #38#)
                    (type list #38#))
           (locally
            (declare (sb-ext:muffle-conditions sb-ext:code-deletion-note))
            (cond ((null #38#) 'nil)
                  ((and (consp #38#) t t)
                   (let ((#39=#:g38 (car #38#)) (#40=#:g39 (cdr #38#)))
                     (declare (ignorable #39# #40#)
                              (type t #39#)
                              (type list #40#))
                     (coalton:cons (coalton/classes:tuple #36# #39#)
                                   (coalton-benchmark/large-inference::q-enumerate-onto
                                    #35#
                                    (coalton/classes:+ #35# #36#
                                                       (coalton/classes:fromint
                                                        #35# 1))
                                    #40#))))
                  (t (error #5#)))))))
  (setf coalton-benchmark/large-inference::q-enumerate-onto
          #'coalton-benchmark/large-inference::q-enumerate-onto))
 (locally
  (declare (optimize (sb-c::type-check 0)))
  (coalton-impl/global-lexical:define-global-lexical
   coalton-benchmark/large-inference::q-enumerate function))
 (declaim
  (ftype (function (coalton/classes::class/num list) (values list &optional))
   coalton-benchmark/large-inference::q-enumerate))
 (locally
  (declare (optimize (sb-c::type-check 0)))
  (defun coalton-benchmark/large-inference::q-enumerate (#41=#:g40 #42=#:g41)
    (declare (ignorable #41# #42#))
    (the (values list &optional)
         (coalton-benchmark/large-inference::q-enumerate-onto #41#
          (coalton/classes:fromint #41# 0) #42#)))
  (setf coalton-benchmark/large-inference::q-enumerate
          #'coalton-benchmark/large-inference::q-enumerate))
 (locally
  (declare (optimize (sb-c::type-check 0)))
  (coalton-impl/global-lexical:define-global-lexical
   coalton-benchmark/large-inference::q-pipeline function))
 (declaim
  (ftype
   (function
    (coalton/math/integral::class/remainder coalton/classes::class/num
                                            coalton/classes::class/ord t)
    (values coalton/classes:tuple &optional))
   coalton-benchmark/large-inference::q-pipeline))
 (locally
  (declare (optimize (sb-c::type-check 0)))
  (defun coalton-benchmark/large-inference::q-pipeline
         (#43=#:g42 #44=#:g43 #45=#:g44 #46=#:g45)
    (declare (ignorable #43# #44# #45# #46#))
    (the (values coalton/classes:tuple &optional)
         (let ((#47=#:g46
                (coalton-benchmark/large-inference::q-range-down
                 (coalton/math/integral::class/remainder-super-0 #43#) #46#)))
           (declare (ignorable #47#))
           (let ((#48=#:g47
                  (coalton-benchmark/large-inference::q-filter
                   (lambda (#49=#:g48)
                     (declare (ignorable #49#)
                              (type t #49#)
                              (values boolean &optional))
                     (the (values boolean &optional)
                          (coalton/classes:==
                           (coalton/classes::class/num-super-0
                            (coalton/math/integral::class/remainder-super-0
                             #43#))
                           (coalton/classes:fromint
                            (coalton/math/integral::class/remainder-super-0
                             #43#)
                            0)
                           (coalton/math/integral:mod #43# #49#
                                                      (coalton/classes:fromint
                                                       (coalton/math/integral::class/remainder-super-0
                                                        #43#)
                                                       2)))))
                   #47#)))
             (declare (ignorable #48#))
             (let ((#50=#:g49
                    (coalton-benchmark/large-inference::q-map
                     (lambda (#51=#:g50)
                       (declare (ignorable #51#)
                                (type t #51#)
                                (values t &optional))
                       (the (values t &optional)
                            (coalton/classes:*
                             (coalton/math/integral::class/remainder-super-0
                              #43#)
                             #51# #51#)))
                     #48#)))
               (declare (ignorable #50#))
               (let ((#52=#:g51
                      (coalton-benchmark/large-inference::q-enumerate
                       coalton/math/num::|INSTANCE/CLS:NUM INTEGER| #50#)))
                 (declare (ignorable #52#))
                 (coalton/classes:tuple
                  (coalton-benchmark/large-inference::q-sum
                   (coalton/math/integral::class/remainder-super-0 #43#) #50#)
                  (coalton/classes:tuple
                   (coalton-benchmark/large-inference::q-len #44# #52#)
                   (coalton-benchmark/large-inference::q-maximum-onto #45#
                    (coalton/classes:fromint
                     (coalton/math/integral::class/remainder-super-0 #43#) 0)
                    (coalton-benchmark/large-inference::q-map coalton/tuple:snd
                     #52#))))))))))
  (setf coalton-benchmark/large-inference::q-pipeline
          #'coalton-benchmark/large-inference::q-pipeline))
 (locally
  (declare (optimize (sb-c::type-check 0)))
  (coalton-impl/global-lexical:define-global-lexical
   coalton-benchmark/large-inference::q-apply-n function))
 (declaim
  (ftype
   (function (coalton/classes::class/num t function t) (values t &optional))
   coalton-benchmark/large-inference::q-apply-n))
 (locally
  (declare (optimize (sb-c::type-check 0)))
  (defun coalton-benchmark/large-inference::q-apply-n
         (#53=#:g52 #54=#:g53 #55=#:g54 #56=#:g55)
    (declare (ignorable #53# #54# #55# #56#))
    (the (values t &optional)
         (if (coalton/classes:== (coalton/classes::class/num-super-0 #53#) #54#
                                 (coalton/classes:fromint #53# 0))
             #56#
             (coalton-benchmark/large-inference::q-apply-n #53#
              (coalton/classes:- #53# #54# (coalton/classes:fromint #53# 1))
              #55# (funcall #55# #56#)))))
  (setf coalton-benchmark/large-inference::q-apply-n
          #'coalton-benchmark/large-inference::q-apply-n))
 (locally
  (declare (optimize (sb-c::type-check 0)))
  (coalton-impl/global-lexical:define-global-lexical
   coalton-benchmark/large-inference::q-compose function))
 (declaim
  (ftype (function (function function) (values function &optional))
   coalton-benchmark/large-inference::q-compose))
 (locally
  (declare (optimize (sb-c::type-check 0)))
  (defun coalton-benchmark/large-inference::q-compose (#57=#:g56 #58=#:g57)
    (declare (ignorable #57# #58#))
    (the (values function &optional)
         (lambda (#59=#:g58)
           (declare (ignorable #59#)
                    (type t #59#)
                    (values t &optional))
           (the (values t &optional) (funcall #57# (funcall #58# #59#))))))
  (setf coalton-benchmark/large-inference::q-compose
          #'coalton-benchmark/large-inference::q-compose))
 (locally
  (declare (optimize (sb-c::type-check 0)))
  (coalton-impl/global-lexical:define-global-lexical
   coalton-benchmark/large-inference::q-twice function))
 (declaim
  (ftype (function (function) (values function &optional))
   coalton-benchmark/large-inference::q-twice))
 (locally
  (declare (optimize (sb-c::type-check 0)))
  (defun coalton-benchmark/large-inference::q-twice (#60=#:g59)
    (declare (ignorable #60#))
    (the (values function &optional)
         (coalton-benchmark/large-inference::q-compose #60# #60#)))
  (setf coalton-benchmark/large-inference::q-twice
          #'coalton-benchmark/large-inference::q-twice))
 (locally
  (declare (optimize (sb-c::type-check 0)))
  (coalton-impl/global-lexical:define-global-lexical
   coalton-benchmark/large-inference::q-count function))
 (declaim
  (ftype
   (function (coalton/classes::class/num function list) (values t &optional))
   coalton-benchmark/large-inference::q-count))
 (locally
  (declare (optimize (sb-c::type-check 0)))
  (defun coalton-benchmark/large-inference::q-count
         (#61=#:g60 #62=#:g61 #63=#:g62)
    (declare (ignorable #61# #62# #63#))
    (the (values t &optional)
         (coalton-benchmark/large-inference::q-foldl
          (lambda (#64=#:g63 #65=#:g64)
            (declare (ignorable #64# #65#)
                     (type t #64#)
                     (type t #65#)
                     (values t &optional))
            (the (values t &optional)
                 (if (funcall #62# #65#)
                     (coalton/classes:+ #61# #64#
                                        (coalton/classes:fromint #61# 1))
                     #64#)))
          (coalton/classes:fromint #61# 0) #63#)))
  (setf coalton-benchmark/large-inference::q-count
          #'coalton-benchmark/large-inference::q-count))
 (locally
  (declare (optimize (sb-c::type-check 0)))
  (coalton-impl/global-lexical:define-global-lexical
   coalton-benchmark/large-inference::q-app function))
 (declaim
  (ftype (function (list list) (values list &optional))
   coalton-benchmark/large-inference::q-app))
 (locally
  (declare (optimize (sb-c::type-check 0)))
  (defun coalton-benchmark/large-inference::q-app (#66=#:g65 #67=#:g66)
    (declare (ignorable #66# #67#))
    (the (values list &optional)
         (let ((#68=#:g67 #66#))
           (declare (ignorable #68#)
                    (type list #68#))
           (locally
            (declare (sb-ext:muffle-conditions sb-ext:code-deletion-note))
            (cond ((null #68#) #67#)
                  ((and (consp #68#) t t)
                   (let ((#69=#:g68 (car #68#)) (#70=#:g69 (cdr #68#)))
                     (declare (ignorable #69# #70#)
                              (type t #69#)
                              (type list #70#))
                     (coalton:cons #69#
                                   (coalton-benchmark/large-inference::q-app
                                    #70# #67#))))
                  (t (error #5#)))))))
  (setf coalton-benchmark/large-inference::q-app
          #'coalton-benchmark/large-inference::q-app))
 (locally
  (declare (optimize (sb-c::type-check 0)))
  (coalton-impl/global-lexical:define-global-lexical
   coalton-benchmark/large-inference::q-concat-map function))
 (declaim
  (ftype (function (function list) (values list &optional))
   coalton-benchmark/large-inference::q-concat-map))
 (locally
  (declare (optimize (sb-c::type-check 0)))
  (defun coalton-benchmark/large-inference::q-concat-map (#71=#:g70 #72=#:g71)
    (declare (ignorable #71# #72#))
    (the (values list &optional)
         (let ((#73=#:g72 #72#))
           (declare (ignorable #73#)
                    (type list #73#))
           (locally
            (declare (sb-ext:muffle-conditions sb-ext:code-deletion-note))
            (cond ((null #73#) 'nil)
                  ((and (consp #73#) t t)
                   (let ((#74=#:g73 (car #73#)) (#75=#:g74 (cdr #73#)))
                     (declare (ignorable #74# #75#)
                              (type t #74#)
                              (type list #75#))
                     (coalton-benchmark/large-inference::q-app
                      (funcall #71# #74#)
                      (coalton-benchmark/large-inference::q-concat-map #71#
                       #75#))))
                  (t (error #5#)))))))
  (setf coalton-benchmark/large-inference::q-concat-map
          #'coalton-benchmark/large-inference::q-concat-map))
 (locally
  (declare (optimize (sb-c::type-check 0)))
  (coalton-impl/global-lexical:define-global-lexical
   coalton-benchmark/large-inference::q-zip-with function))
 (declaim
  (ftype (function (function list list) (values list &optional))
   coalton-benchmark/large-inference::q-zip-with))
 (locally
  (declare (optimize (sb-c::type-check 0)))
  (defun coalton-benchmark/large-inference::q-zip-with
         (#76=#:g75 #77=#:g76 #78=#:g77)
    (declare (ignorable #76# #77# #78#))
    (the (values list &optional)
         (let ((#79=#:g78 (coalton/classes:tuple #77# #78#)))
           (declare (ignorable #79#)
                    (type coalton/classes:tuple #79#))
           (locally
            (declare (sb-ext:muffle-conditions sb-ext:code-deletion-note))
            (cond
             ((and (typep #79# 'coalton/classes::tuple/tuple)
                   (and (consp (coalton/classes::tuple/tuple-_0 #79#)) t t)
                   (and (consp (coalton/classes::tuple/tuple-_1 #79#)) t t))
              (let ((#80=#:g79 (car (coalton/classes::tuple/tuple-_0 #79#)))
                    (#81=#:g80 (cdr (coalton/classes::tuple/tuple-_0 #79#)))
                    (#82=#:g81 (car (coalton/classes::tuple/tuple-_1 #79#)))
                    (#83=#:g82 (cdr (coalton/classes::tuple/tuple-_1 #79#))))
                (declare (ignorable #80# #81# #82# #83#)
                         (type t #80#)
                         (type list #81#)
                         (type t #82#)
                         (type list #83#))
                (coalton:cons (funcall #76# #80# #82#)
                              (coalton-benchmark/large-inference::q-zip-with
                               #76# #81# #83#))))
             (t 'nil))))))
  (setf coalton-benchmark/large-inference::q-zip-with
          #'coalton-benchmark/large-inference::q-zip-with))
 (locally
  (declare (optimize (sb-c::type-check 0)))
  (coalton-impl/global-lexical:define-global-lexical
   coalton-benchmark/large-inference::q-replicate function))
 (declaim
  (ftype (function (coalton/classes::class/num t t) (values list &optional))
   coalton-benchmark/large-inference::q-replicate))
 (locally
  (declare (optimize (sb-c::type-check 0)))
  (defun coalton-benchmark/large-inference::q-replicate
         (#84=#:g83 #85=#:g84 #86=#:g85)
    (declare (ignorable #84# #85# #86#))
    (the (values list &optional)
         (if (coalton/classes:== (coalton/classes::class/num-super-0 #84#) #85#
                                 (coalton/classes:fromint #84# 0))
             'nil
             (coalton:cons #86#
                           (coalton-benchmark/large-inference::q-replicate #84#
                            (coalton/classes:- #84# #85#
                                               (coalton/classes:fromint #84#
                                                                        1))
                            #86#)))))
  (setf coalton-benchmark/large-inference::q-replicate
          #'coalton-benchmark/large-inference::q-replicate))
 (locally
  (declare (optimize (sb-c::type-check 0)))
  (coalton-impl/global-lexical:define-global-lexical
   coalton-benchmark/large-inference::q-drop function))
 (declaim
  (ftype (function (coalton/classes::class/num t list) (values list &optional))
   coalton-benchmark/large-inference::q-drop))
 (locally
  (declare (optimize (sb-c::type-check 0)))
  (defun coalton-benchmark/large-inference::q-drop
         (#87=#:g86 #88=#:g87 #89=#:g88)
    (declare (ignorable #87# #88# #89#))
    (the (values list &optional)
         (if (coalton/classes:== (coalton/classes::class/num-super-0 #87#) #88#
                                 (coalton/classes:fromint #87# 0))
             #89#
             (let ((#90=#:g89 #89#))
               (declare (ignorable #90#)
                        (type list #90#))
               (locally
                (declare (sb-ext:muffle-conditions sb-ext:code-deletion-note))
                (cond ((null #90#) 'nil)
                      ((and (consp #90#) t t)
                       (let ((#91=#:g90 (cdr #90#)))
                         (declare (ignorable #91#)
                                  (type list #91#))
                         (coalton-benchmark/large-inference::q-drop #87#
                          (coalton/classes:- #87# #88#
                                             (coalton/classes:fromint #87# 1))
                          #91#)))
                      (t (error #5#))))))))
  (setf coalton-benchmark/large-inference::q-drop
          #'coalton-benchmark/large-inference::q-drop))
 (locally
  (declare (optimize (sb-c::type-check 0)))
  (coalton-impl/global-lexical:define-global-lexical
   coalton-benchmark/large-inference::q-take function))
 (declaim
  (ftype (function (coalton/classes::class/num t list) (values list &optional))
   coalton-benchmark/large-inference::q-take))
 (locally
  (declare (optimize (sb-c::type-check 0)))
  (defun coalton-benchmark/large-inference::q-take
         (#92=#:g91 #93=#:g92 #94=#:g93)
    (declare (ignorable #92# #93# #94#))
    (the (values list &optional)
         (if (coalton/classes:== (coalton/classes::class/num-super-0 #92#) #93#
                                 (coalton/classes:fromint #92# 0))
             'nil
             (let ((#95=#:g94 #94#))
               (declare (ignorable #95#)
                        (type list #95#))
               (locally
                (declare (sb-ext:muffle-conditions sb-ext:code-deletion-note))
                (cond ((null #95#) 'nil)
                      ((and (consp #95#) t t)
                       (let ((#96=#:g95 (car #95#)) (#97=#:g96 (cdr #95#)))
                         (declare (ignorable #96# #97#)
                                  (type t #96#)
                                  (type list #97#))
                         (coalton:cons #96#
                                       (coalton-benchmark/large-inference::q-take
                                        #92#
                                        (coalton/classes:- #92# #93#
                                                           (coalton/classes:fromint
                                                            #92# 1))
                                        #97#))))
                      (t (error #5#))))))))
  (setf coalton-benchmark/large-inference::q-take
          #'coalton-benchmark/large-inference::q-take))
 (locally
  (declare (optimize (sb-c::type-check 0)))
  (coalton-impl/global-lexical:define-global-lexical
   coalton-benchmark/large-inference::q-any function))
 (declaim
  (ftype (function (function list) (values boolean &optional))
   coalton-benchmark/large-inference::q-any))
 (locally
  (declare (optimize (sb-c::type-check 0)))
  (defun coalton-benchmark/large-inference::q-any (#98=#:g97 #99=#:g98)
    (declare (ignorable #98# #99#))
    (the (values boolean &optional)
         (let ((#100=#:g99 #99#))
           (declare (ignorable #100#)
                    (type list #100#))
           (locally
            (declare (sb-ext:muffle-conditions sb-ext:code-deletion-note))
            (cond ((null #100#) 'nil)
                  ((and (consp #100#) t t)
                   (let ((#101=#:g100 (car #100#)) (#102=#:g101 (cdr #100#)))
                     (declare (ignorable #101# #102#)
                              (type t #101#)
                              (type list #102#))
                     (if (funcall #98# #101#)
                         't
                         (coalton-benchmark/large-inference::q-any #98#
                          #102#))))
                  (t (error #5#)))))))
  (setf coalton-benchmark/large-inference::q-any
          #'coalton-benchmark/large-inference::q-any))
 (locally
  (declare (optimize (sb-c::type-check 0)))
  (coalton-impl/global-lexical:define-global-lexical
   coalton-benchmark/large-inference::q-all function))
 (declaim
  (ftype (function (function list) (values boolean &optional))
   coalton-benchmark/large-inference::q-all))
 (locally
  (declare (optimize (sb-c::type-check 0)))
  (defun coalton-benchmark/large-inference::q-all (#103=#:g102 #104=#:g103)
    (declare (ignorable #103# #104#))
    (the (values boolean &optional)
         (let ((#105=#:g104 #104#))
           (declare (ignorable #105#)
                    (type list #105#))
           (locally
            (declare (sb-ext:muffle-conditions sb-ext:code-deletion-note))
            (cond ((null #105#) 't)
                  ((and (consp #105#) t t)
                   (let ((#106=#:g105 (car #105#)) (#107=#:g106 (cdr #105#)))
                     (declare (ignorable #106# #107#)
                              (type t #106#)
                              (type list #107#))
                     (if (funcall #103# #106#)
                         (coalton-benchmark/large-inference::q-all #103# #107#)
                         'nil)))
                  (t (error #5#)))))))
  (setf coalton-benchmark/large-inference::q-all
          #'coalton-benchmark/large-inference::q-all))
 (locally
  (declare (optimize (sb-c::type-check 0)))
  (coalton-impl/global-lexical:define-global-lexical
   coalton-benchmark/large-inference::q-product function))
 (declaim
  (ftype (function (coalton/classes::class/num list) (values t &optional))
   coalton-benchmark/large-inference::q-product))
 (locally
  (declare (optimize (sb-c::type-check 0)))
  (defun coalton-benchmark/large-inference::q-product (#108=#:g107 #109=#:g108)
    (declare (ignorable #108# #109#))
    (the (values t &optional)
         (coalton-benchmark/large-inference::q-foldl
          (lambda (#110=#:g109 #111=#:g110)
            (declare (ignorable #110# #111#)
                     (type t #110#)
                     (type t #111#)
                     (values t &optional))
            (the (values t &optional) (coalton/classes:* #108# #110# #111#)))
          (coalton/classes:fromint #108# 1) #109#)))
  (setf coalton-benchmark/large-inference::q-product
          #'coalton-benchmark/large-inference::q-product))
 (locally
  (declare (optimize (sb-c::type-check 0)))
  (coalton-impl/global-lexical:define-global-lexical
   coalton-benchmark/large-inference::q-rev-onto function))
 (declaim
  (ftype (function (list list) (values list &optional))
   coalton-benchmark/large-inference::q-rev-onto))
 (locally
  (declare (optimize (sb-c::type-check 0)))
  (defun coalton-benchmark/large-inference::q-rev-onto
         (#112=#:g111 #113=#:g112)
    (declare (ignorable #112# #113#))
    (the (values list &optional)
         (let ((#114=#:g113 #113#))
           (declare (ignorable #114#)
                    (type list #114#))
           (locally
            (declare (sb-ext:muffle-conditions sb-ext:code-deletion-note))
            (cond ((null #114#) #112#)
                  ((and (consp #114#) t t)
                   (let ((#115=#:g114 (car #114#)) (#116=#:g115 (cdr #114#)))
                     (declare (ignorable #115# #116#)
                              (type t #115#)
                              (type list #116#))
                     (coalton-benchmark/large-inference::q-rev-onto
                      (coalton:cons #115# #112#) #116#)))
                  (t (error #5#)))))))
  (setf coalton-benchmark/large-inference::q-rev-onto
          #'coalton-benchmark/large-inference::q-rev-onto))
 (locally
  (declare (optimize (sb-c::type-check 0)))
  (coalton-impl/global-lexical:define-global-lexical
   coalton-benchmark/large-inference::q-rev function))
 (declaim
  (ftype (function (list) (values list &optional))
   coalton-benchmark/large-inference::q-rev))
 (locally
  (declare (optimize (sb-c::type-check 0)))
  (defun coalton-benchmark/large-inference::q-rev (#117=#:g116)
    (declare (ignorable #117#))
    (the (values list &optional)
         (coalton-benchmark/large-inference::q-rev-onto 'nil #117#)))
  (setf coalton-benchmark/large-inference::q-rev
          #'coalton-benchmark/large-inference::q-rev))
 (declaim (sb-ext:unmuffle-conditions sb-kernel:redefinition-warning))
 (values))" :cost (:parse (:min-seconds 0.001873d0 :median-seconds 0.001892d0 :bytes 425840 :iterations 5) :typecheck (:min-seconds 0.296458d0 :median-seconds 0.304714d0 :bytes 90039040 :iterations 5) :codegen (:min-seconds 0.017441d0 :median-seconds 0.018199d0 :bytes 3272144 :iterations 5))))

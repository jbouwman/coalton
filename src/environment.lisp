(defpackage #:coalton-impl/environment
  (:use
   #:cl)
  (:shadow
   #:get
   #:set
   #:update
   #:map)
  (:export
   #:get
   #:keys
   #:set
   #:update
   #:unset
   #:map))

(in-package #:coalton-impl/environment)

;; There are 14 namespaces in the environment.

(defvar *types*
  '(:class
    :code
    :constructor
    :function
    :fundep
    :instance
    :codegen-sym ; "codegen symbol" -> instance ; HELP what exactly is a "codegen symbol"?
    :method-inline
    :name                               ; K -> V

    ;; maps the names of user-defined functions to a list of their
    ;; user-supplied parameter names, for documentation generation.

    :source-name
    :specialization
    :struct
    :type
    :value))

(defun %check-type (type)
  (or (find type *types*)
      (error "Illegal type ~A" type)))

(defstruct environment
  type                                  ; which of the 13
  operation                             ; set/unset/add/etc
  key                                   ; symbol
  value
  cache                                 ; readthrough cache
  parent)                               ; functional parent

(defmethod print-object ((env environment) stream)
  (declare (type stream stream)
           (type environment env))
  (print-unreadable-object (env stream :type t :identity t)))

(defmethod make-load-form ((self environment) &optional env)
  (declare (ignore env))
  (error "TODO: Persist environment efficiently: ~A" self)
  #++ (make-load-form-saving-slots self :environment env))

#+(and sbcl coalton-release)
(declaim (sb-ext:freeze-type environment))

(defun get (env type key)
  (%check-type type)
  (gethash key (type-cache env type)))

(defun set (env type &rest kvs)
  (%check-type type)
  (loop :for (k v) :on kvs :by #'cddr
        :do (setf env (make-environment :type type
                                        :operation :set
                                        :key k
                                        :value v
                                        :cache nil
                                        :parent env)))
  env)

(defun unset (env type &rest ks)
  (%check-type type)
  (loop :for k :in ks
        :do (setf env (make-environment :type type
                                        :operation :unset
                                        :key k
                                        :value nil
                                        :cache nil
                                        :parent env)))
  env)

(defun cache-ops (environment type)
  "Return a vector of operations that will build a cache for TYPE."
  (%check-type type)
  (let ((ops (make-array 0 :adjustable t :fill-pointer t)))
    (loop
      (cond ((not environment)
             (return-from cache-ops ops))
            ((and (environment-cache environment)
                  (gethash type (environment-cache environment)))
             (vector-push-extend (list :cache (gethash type (environment-cache environment)))
                                 ops))
            ((eql type (environment-type environment))
             (vector-push-extend (list (environment-operation environment)
                                       (environment-key environment)
                                       (environment-value environment))
                                 ops)))
      (setf environment (environment-parent environment)))))

;; fixme rename

(defun type-cache (environment type)
  (let ((cache (and (environment-cache environment)
                    (gethash type (environment-cache environment)))))
    (when cache
      (return-from type-cache cache))
    (setf cache (make-hash-table))
    (let ((ops (cache-ops environment type)))
      (loop :for i :downfrom (1- (length ops)) :to 0
            :do (let ((op (aref ops i)))
                  (ecase (car op)
                    (:set (setf (gethash (cadr op) cache) (caddr op)))))))
    (unless (environment-cache environment)
      (setf (environment-cache environment) (make-hash-table)))
    (setf (gethash type (environment-cache environment)) cache)
    cache))

(defun map (env type f)
  "Map over k->v bindings in type namespace."
  (%check-type type)
  (maphash f (type-cache env type)))

(defun keys (env type)
  (let ((ks (make-array 0 :adjustable t :fill-pointer t)))
    (map env type (lambda (k v)
                    (declare (ignore v))
                    (vector-push-extend k ks)))
    (coerce ks 'list)))

(defun update (env type f)
  (%check-type type)
  (maphash (lambda (k v)
             (setf env
                   (set env type k (funcall f v))))
           (type-cache env type))
  env)

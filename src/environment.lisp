(defpackage #:coalton-impl/environment
  (:use
   #:cl)
  (:shadow
   #:get
   #:set
   #:update
   #:map)
  (:export
   #:empty
   #:environment
   #:get
   #:keys
   #:set
   #:set*
   #:update
   #:unset
   #:map))

(in-package #:coalton-impl/environment)

(defstruct environment
  (type      nil :type symbol)          ; namespace
  (operation nil :type symbol)          ; set/unset
  operands                              ; ks/kvs
  ptr                                   ; parent
  map)                                  ; namespace->k->v

(defmethod print-object ((env environment) stream)
  (declare (type stream stream)
           (type environment env))
  (print-unreadable-object (env stream :type t :identity t)
    ;; ...
    ))

(defmethod make-load-form ((self environment) &optional env)
  (make-load-form-saving-slots self :environment env))

(defun empty ()
  (make-environment :map (fset:empty-map)))

(defun type-environment (env type)
  (or (fset:lookup (environment-map env) type)
      (fset:empty-map)))

(defun get (env type k)
  (fset:lookup (type-environment env type) k))

(defun set (env type k v)
  (let ((map (environment-map env)))
    (make-environment :type type
                      :operation :set
                      :operands (list k v)
                      :ptr env
                      :map (fset:with map
                                      type (fset:with (or (fset:lookup map type)
                                                          (fset:empty-map))
                                                      k v)))))

(defun set* (env type &rest kvs)
  (loop :for (k v) :on kvs :by #'cddr
        :do (setf env (set env type k v)))
  env)

(defun unset (env type k)
  (let ((map (environment-map env)))
    (make-environment :type type
                      :operation :unset
                      :operands (list k)
                      :ptr env
                      :map (fset:with map
                                      type (fset:less (or (fset:lookup map type)
                                                          (fset:empty-map))
                                                      k)))))

(defun keys (env type)
  (fset:convert 'list
                (fset:domain (type-environment env type))))

(defun map (env type f)
  "Map over k->v bindings in type namespace."
  (dolist (k (keys env type))
    (funcall f k (get env type k))))

(defun update (env type f)
  (reduce (lambda (env k)
            (set env type k (funcall f (get env type k))))
          (keys env type)
          :initial-value env))

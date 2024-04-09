(defpackage #:coalton-impl/environment
  (:use
   #:cl)
  (:shadow
   #:get
   #:set
   #:update
   #:map)
  (:export
   #:do-env
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
  (ns nil :type symbol)                 ; namespace
  op                                    ; operation
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

(defun type-environment (env ns)
  (or (fset:lookup (environment-map env) ns)
      (fset:empty-map)))

(defun get (env ns k)
  (fset:lookup (type-environment env ns) k))

(defun set (env ns k v)
  (let ((map (environment-map env)))
    (make-environment :ns ns
                      :op (list :set k v)
                      :ptr env
                      :map (fset:with map
                                      ns (fset:with (or (fset:lookup map ns)
                                                        (fset:empty-map))
                                                    k v)))))

(defun set* (env ns &rest kvs)
  (loop :for (k v) :on kvs :by #'cddr
        :do (setf env (set env ns k v)))
  env)

(defun unset (env ns k)
  (let ((map (environment-map env)))
    (make-environment :ns ns
                      :op (list :unset k)
                      :ptr env
                      :map (fset:with map
                                      ns (fset:less (or (fset:lookup map ns)
                                                        (fset:empty-map))
                                                    k)))))

(defun keys (env ns)
  (fset:convert 'list (fset:domain (type-environment env ns))))

(defun map (env ns f)
  "Map (f k v) bindings in ns of env."
  (dolist (k (keys env ns))
    (funcall f k (get env ns k))))

(defmacro do-env ((k v env ns) &body body)
  `(map ,env ,ns (lambda (,k ,v) ,@body)))

(defun update (env ns f)
  (reduce (lambda (env k)
            (set env ns k (funcall f (get env ns k))))
          (keys env ns)
          :initial-value env))

(in-package #:coalton-tests)

;; Test that error messages containing source spans are correctly
;; printed.

(defvar *program-text*
  "  ;;
  ;; Kinds
  ;;

  (define-type Kind
    Star
    (Kfun Kind Kind))

  (define-instance (Eq Kind)
    (define (== k1 k2)
      (match (Tuple k1 k2)
        ((Tuple (Star) (Star)) True)
        ((Tuple (Kfun a1 a2)
                (Kfun b1 b2))
         (and (== a1 b1)
              (== a2 b2)))
        (_ False))))
")

(deftest stream-access ()
  (with-input-from-string (stream *program-text*)
    (is (equal '(0 5 16 21 22 42 51 73 74 103 126 153 190 219 249 274 301 322)
               (source-error::find-line-offsets stream))))
  (with-test-error-state state
    (is (string= *program-text*
                 (format nil "~{~A~^~%~}" 
                         (loop :for i :from 1
                                 :to (length (slot-value state 'source-error::line-offsets))
                               :collect (source-error::line-contents state i)))))))

(defun make-test-error (coalton-file)
  (coalton-impl/error::coalton-error :span '(76 . 321)
                                     :file coalton-file
                                     :message "message"
                                     :primary-note "define instance form"
                                     :notes (list
                                             (coalton-impl/error:make-coalton-error-note
                                              :type :secondary
                                              :span  '(132 . 319)
                                              :message "message 2")
                                             (coalton-impl/error:make-coalton-error-note
                                              :type :secondary
                                              :span  '(140 . 145)
                                              :message "message 3")
                                             (coalton-impl/error:make-coalton-error-note
                                              :type :secondary
                                              :span  '(170 . 174)
                                              :message "message 4"))
                                     :help-notes
                                     (list
                                      (coalton-impl/error:make-coalton-error-help
                                       :span  '(289 . 291)
                                       :replacement (lambda (existing)
                                                      (concatenate 'string "*" existing "*"))
                                       :message "message 5"))))

(defmacro with-test-error (error-info &body body)
  `(uiop:with-temporary-file (:stream output-stream
                              :pathname program-file
                              :suffix "coalton"
                              :direction :output)
     (write-string *program-text* output-stream)
     :close-stream
     (with-open-file (stream program-file)
       (let* ((f (coalton-impl/error::make-coalton-ide-file program-file "file" 0))
              (,error-info (make-test-error f)))
         ,@body))))

(defmacro with-test-error-state (state &body body)
  `(with-test-error error
     (source-error::with-source-stream (source-stream error)
       (let ((,state (make-instance 'source-error::report-state
                                    :source-stream source-stream
                                    :error-info error)))
         ,@body))))
  (slot-value state 'source-error::offset-positions))

(defun test-error-string ()
  (with-test-error error
    (with-output-to-string (stream)
      (source-error:print-source-error stream error))))

(deftest test-error ()
  (is (string= (test-error-string)
               "error: message
  --> file:9:2
    |
 9  |      (define-instance (Eq Kind)
    |  ____^
 10 | |      (define (== k1 k2)
 11 | |        (match (Tuple k1 k2)
    | | _______-
    | ||               ----- message 3
 12 | ||         ((Tuple (Star) (Star)) True)
    | ||                  ---- message 4
 13 | ||         ((Tuple (Kfun a1 a2)
 ...
 16 | ||               (== a2 b2)))
 17 | ||         (_ False))))
    | ||__________________- message 2
    | |_____________________^ define instance form
help: message 5
 16 |               (*==* a2 b2)))
    |                ----
")))

(in-package #:coalton-tests)

;; Test that error messages containing source spans are correctly
;; printed.

(deftest test-printer-cols ()
  (let ((state (make-instance 'source-error/error::printer-state)))

    (setf (source-error/error::line-number-width state) 2)

    (check-string= "Line number column"
                   (with-output-to-string (stream)
                     (source-error/error::print-prefix stream state))
                   "    |")

    (check-string= "Line number column"
                   (with-output-to-string (stream)
                     (source-error/error::print-prefix stream state 1))
                   " 1  |")

    (check-string= "Line number column"
                   (with-output-to-string (stream)
                     (source-error/error::print-prefix stream state 76))
                   " 76 |")

    (setf (source-error/error::note-max-depth state) 2
          (source-error/error::note-stack state) '())

    (check-string= "Arrow column"
                   (with-output-to-string (stream)
                     (source-error/error::print-margin stream state))
                   "   ")

    (setf (source-error/error::note-stack state) '(x))

    (check-string= "Arrow column"
                   (with-output-to-string (stream)
                     (source-error/error::print-margin stream state))
                   " | ")

    (setf (source-error/error::note-stack state) '(x x))

    (check-string= "Arrow column"
                   (with-output-to-string (stream)
                     (source-error/error::print-margin stream state))
                   " ||")))

(defvar *error-example* ";;
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
      (_ False))))")

(defvar *numbered-example* " 1  | ;;
 2  | ;; Kinds
 3  | ;;
 4  | 
 5  | (define-type Kind
 6  |   Star
 7  |   (Kfun Kind Kind))
 8  | 
 9  | (define-instance (Eq Kind)
 10 |   (define (== k1 k2)
 11 |     (match (Tuple k1 k2)
 12 |       ((Tuple (Star) (Star)) True)
 13 |       ((Tuple (Kfun a1 a2)
 14 |               (Kfun b1 b2))
 15 |        (and (== a1 b1)
 16 |             (== a2 b2)))
 17 |       (_ False))))
")

(defun dump-lines (state)
  (with-output-to-string (output)
    (loop :for line :from 1 :to (source-error/error::last-line state)
          :do (source-error/error::print-line output state line))))

(deftest test-printer-state ()
  (let ((source (source:make-source-string *error-example* :name "file")))
    (with-open-stream (stream (source-error:source-stream source))
      (let ((state (make-instance 'source-error/error::printer-state :source-stream stream)))
        (check-string= "Numbered source, no notes"
                       (dump-lines state)
                       *numbered-example*)))))

(defvar *single-note*
  " 9  |   (define-instance (Eq Kind)
    |  _^
 10 | |   (define (== k1 k2)
 ...
 16 | |             (== a2 b2)))
 17 | |       (_ False))))
    | |__________________^ define instance form
")

(deftest test-printer-state-2 ()
  (let* ((source (source:make-source-string *error-example* :name "file"))
         (note (source:note (source:make-location source '(62 . 291))
                            "define instance form")))
    (with-open-stream (stream (source-error:source-stream source))
      (let ((state (make-instance 'source-error/error::printer-state
                     :source-stream stream
                     :notes (list note))))
        (check-string= "Single note"
                       (with-output-to-string (output)
                         (source-error/error::print-state output state))
                       *single-note*)))))

(defvar *double-note*
  " 9  |    (define-instance (Eq Kind)
    |  __^
 10 | |    (define (== k1 k2)
 11 | |      (match (Tuple k1 k2)
    | | _____-
 12 | ||       ((Tuple (Star) (Star)) True)
 ...
 16 | ||             (== a2 b2)))
 17 | ||       (_ False))))
    | ||________________- define form
    | |___________________^ define instance form
")


(deftest test-printer-state-3 ()
  (let* ((source (source:make-source-string *error-example* :name "file"))
         (note1 (source:note (source:make-location source '(62 . 291))
                             "define instance form"))
         (note2 (source:secondary (source:make-location source '(114 . 289))
                             "define form")))
    (with-open-stream (stream (source-error:source-stream source))
      (let ((state (make-instance 'source-error/error::printer-state
                     :source-stream stream
                     :notes (list note1 note2))))
        (check-string= "Double note"
                       (with-output-to-string (output)
                         (source-error/error::print-state output state))
                       *double-note*)))))

(deftest test-error ()
  (let ((source (source:make-source-string *error-example* :name "file")))
    (handler-case
        ;; an annotating error
        (se:source-error "message"
                         (source:note (source:make-location source '(62 . 291))
                                      "define instance form")
                         (source:secondary (source:make-location source '(114 . 289))
                                           "message 2")
                         (source:secondary (source:make-location source '(122 . 127))
                                           "message 3")
                         (source:secondary (source:make-location source '(150 . 154))
                                           "message 4")
                         (source:help (source:make-location source '(261 . 263))
                                      (lambda (existing)
                                        (concatenate 'string "*" existing "*"))
                                      "message 5"))
      (source-error:source-condition (c)
        (check-string= "Full error example"
                       (princ-to-string c)
                       "error: message
  --> file:9:0
    |
 9  |    (define-instance (Eq Kind)
    |  __^
 10 | |    (define (== k1 k2)
 11 | |      (match (Tuple k1 k2)
    | | _____-
    | ||             ----- message 3
 12 | ||       ((Tuple (Star) (Star)) True)
    | ||                ---- message 4
 13 | ||       ((Tuple (Kfun a1 a2)
 ...
 16 | ||             (== a2 b2)))
 17 | ||       (_ False))))
    | ||________________- message 2
    | |___________________^ define instance form
help: message 5
 16 |             (*==* a2 b2)))
    |              ----
")))))

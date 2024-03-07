(defpackage #:source-error
  (:use
   #:cl)
  (:export
   #:context
   #:deferred-note
   #:help
   #:note
   #:print-source-error                 ; FUNCTION
   #:source-error                       ; FUNCTION, CONDITION
   #:source-location                    ; FUNCTION
   #:source-name                        ; GENERIC
   #:source-stream                      ; GENERIC
   #:source-warning))                   ; FUNCTION

(in-package #:source-error)

;; Source-aware error and warning condition types and helper functions
;;
;; These are the condition classes used by applications. Error and
;; warning conditions both have an `error` slot of type
;; `%source-error` that contains detailed information about the
;; source of the error and optional explanatory notes and
;; messages. `print-source-error` uses this structure to emit an
;; annotated error description when either of these conditions is
;; printed.

;; source info protocol
;;
;; The generic functions `source-name` and `source-stream` define a
;; protocol that gives the condition printer access to an input source
;; related to the condition, via the `source` sl§>ot on a
;; %source-error struct (see below). These provide, respectively,
;; the name, and contents of a source.

;; source annotation structures
;;
;; These are classes that hold for input span-associated error
;; annotations. The top-level `%source-error` structure may
;; contain sets of optional `note` and
;; `source-error-help` messages that refer to source spans, which
;; `print-source-error` will resolve to character ranges in an input
;; source.

(defgeneric source-name (source)
  (:documentation "Returns a string that names a source, for reporting in conditions. In
the case of files, this is the filename path."))

(defgeneric source-stream (source)
  (:documentation "A seekable stream providing access to the original contents of a
source, for reporting in condition printers. WARNING: it is the
responsibility of the caller to close the stream, if not using the
with-source-stream convenience macro."))

(defun report-error (condition stream)
  (with-slots (error) condition
    (print-source-error stream error)))

(define-condition source-condition (condition)
  ((error :initarg :err))
  (:documentation "The type for user-facing errors.")
  (:report report-error))

(define-condition source-error (source-condition error)
  ()
  (:documentation "The type for user-facing errors."))

(define-condition source-warning (source-condition style-warning)
  ()
  (:documentation "The type for user-facing warnings."))

(defclass source-annotation ()
  ((span :initarg :span
         :reader span)
   (message :initarg :message
            :reader note-message)))

(defun span-start (source-annotation)
  (car (slot-value source-annotation 'span)))

(defun span-end (source-annotation)
  (cdr (slot-value source-annotation 'span)))

(defclass note (source-annotation)
  ((type :initarg :type
         :reader note-type)))

(defun note (&key span type message)
  (make-instance 'note
    :span span
    :message message
    :type type))

(defclass help (source-annotation)
  ((replacement :initarg :replacement
                :reader note-replacement)))

(defun help (&key span replacement message)
  (make-instance 'help
    :span span
    :message message
    :replacement replacement))

(defclass deferred-note ()
  ((fn :initarg :fn)
   (rendered)))

(defun deferred-note (fn)
  (make-instance 'deferred-note :fn fn))

(defclass error-context ()
  ((message :initarg :message
            :reader error-context-message)))

(defun context (&key message)
  (make-instance 'error-context :message message))

(defclass %source-error ()
  ((type :initarg :type)
   (source :initarg :source
           :reader error-source)
   (location :initarg :location)
   (message :initarg :message)
   (notes :initarg :notes
          :reader notes)
   (help :initarg :help
         :initform nil)
   (context :initarg :context
            :reader error-context))
  (:documentation "The content of a source-condition, used to produce a printer-state when the error is printed."))

(defun source-error (&key (type :error) source location message primary-note notes help context)
  "Construct a %SOURCE-ERROR instance with a message and primary note attached to the provided form.

MESSAGE and PRIMARY-NOTE must be supplied string arguments.
NOTES and HELP may optionally be supplied notes and help messages."
  (let ((span (etypecase location
                (cons location)
                (integer (cons (1- location) location)))))
    (make-instance '%source-error
      :type type
      :source source
      :location (etypecase location
                  (cons (car location))
                  (integer location))
      :message message
      :notes (list* (note :type :primary
                          :span span
                          :message primary-note)
                    notes)
      :help help
      :context context)))

(defmacro with-source-stream ((stream source-error) &body body)
  `(with-open-stream (,stream (source-stream (error-source ,source-error)))
    ,@body))

;; error formatter
;;
;; `print-source-error`, at the bottom, is a workhorse function that
;; takes a %source-error structure, resolves source-relative spans
;; to the input source, and then prints annotated source code.
;;
;; `printer-state` maintains state during printing: current line,
;; note depth, etc.

(defclass printer-state ()
  ((source-stream :initarg :source-stream
                  :reader source-stream
                  :documentation "The stream form which source code is to be read.")

   (notes :initarg :notes)
   (help :initarg :help)
   (context :initarg :context)

   (line-offsets :reader line-offsets)
   (offset-positions :initform (make-hash-table))

   (line-number-width)
   (current-line)
   (last-line)

   ;; We need to keep track of the current depth of multiline
   ;; notes so that we can pad the left with the correct
   ;; number of columns.

   (note-stack :accessor note-stack
               :initform nil)
   (note-max-depth :initform 0)))

(defun first-line-number (notes offset-positions)
  (car (gethash (span-start (car notes)) offset-positions)))

(defun last-line-number (notes offset-positions)
  (let ((last-offset (apply #'max (mapcar #'span-end notes))))
    (car (gethash last-offset offset-positions))))

(defmethod initialize-instance :after ((printer-state printer-state) &rest initargs)
  (declare (ignore initargs))
  (with-slots (source-stream
               line-offsets
               offset-positions
               note-max-depth
               current-line
               last-line
               line-number-width
               notes)
      printer-state
    (let ((char-offsets (char-offsets printer-state))
          (offsets (find-line-offsets source-stream)))
      (setf line-offsets (make-array (length offsets) :initial-contents offsets))
      (loop :for (char-offset line column) :in (find-column-offsets offsets char-offsets)
            :do (setf (gethash char-offset offset-positions)
                      (cons line column)))
      (setf current-line (1- (first-line-number notes offset-positions))
            last-line (last-line-number notes offset-positions)
            line-number-width (1+ (floor (log last-line 10)))
            note-max-depth (max-depth notes)))))

(defun span-lines (printer-state source-annotation)
  "Return the start and end lines of SOURCE-ANNOTATION"
  (with-slots (offset-positions) printer-state
    (destructuring-bind (start . end) (span source-annotation)
      (values (car (gethash start offset-positions))
              (car (gethash end offset-positions))))))

(defun span-positions (printer-state source-annotation)
  "Return the start and end positions of SOURCE-ANNOTATION"
  (with-slots (offset-positions) printer-state
    (destructuring-bind (start . end) (span source-annotation)
      (destructuring-bind (start-line . start-column)
          (gethash start offset-positions)
        (destructuring-bind (end-line . end-column)
            (gethash end offset-positions)
          (values start-line start-column end-line end-column))))))

;; Mapping between character offsets and line and column positions.
;;
;; First line (zero indexed) = offset 0, etc.

(defun find-line-offsets (stream)
  "Compute the offsets of lines in a stream."
  (file-position stream 0)
  (loop :with index := 0
        :for char := (read-char stream nil nil)
        :unless char
          :return (cons 0 offsets)
        :when (char= char #\Newline)
          :collect (1+ index) :into offsets
        :do (incf index)))

(defun find-column-offsets (line-offsets offsets)
  "Given the offsets of newlines in a stream, compute the line and
column numbers for a sequence of absolute stream offsets."
  (loop :with line := 0
        :with position := 0
        :while offsets
        :when (or (null line-offsets)
                  (< (car offsets)
                     (car line-offsets)))
          :collect (list (car offsets) line (- (car offsets) position))
          :and :do (pop offsets)
        :else
          :do (setf position (car line-offsets)
                    line (1+ line)
                    line-offsets (cdr line-offsets))))

(defun line-contents (printer-state line-number)
  (with-slots (line-offsets source-stream) printer-state
    (let ((offset (if (= 1 line-number)
                      0
                      (aref line-offsets (1- line-number)))))
      (file-position source-stream offset)
      (read-line source-stream nil ""))))

(defun positioned-annotations (printer-state)
  (with-slots (notes help) printer-state
    (concatenate 'list notes help)))

(defun char-offsets (printer-state)
  (sort (remove-duplicates
         (mapcan (lambda (note)
                   (list (span-start note) (span-end note)))
                 (positioned-annotations printer-state)))
        #'<))

(defun start-position (printer-state span)
  (with-slots (offset-positions) printer-state
    (gethash (span-start span) offset-positions)))

(defun start-line (printer-state span)
  (with-slots (offset-positions) printer-state
    (car (gethash (span-start span) offset-positions))))

(defun start-column (printer-state span)
  (with-slots (offset-positions) printer-state
    (cdr (gethash (span-start span) offset-positions))))

(defun end-position (printer-state span)
  (with-slots (offset-positions) printer-state
    (gethash (span-end span) offset-positions)))

(defun end-line (printer-state span)
  (with-slots (offset-positions) printer-state
    (car (gethash (span-end span) offset-positions))))

(defun end-column (printer-state span)
  (with-slots (offset-positions) printer-state
    (cdr (gethash (span-end span) offset-positions))))

(defun span-point< (a b)
  (destructuring-bind (offset-a . type-a) a
    (destructuring-bind (offset-b . type-b) b
      (if (= offset-a offset-b)
          (and (eql type-a :start)
               (eql type-b :end))
          (< offset-a offset-b)))))

(defun span-points (span)
  (list (cons (span-start span) :start)
        (cons (span-end span) :end)))

(defun max-depth (notes)
  (let ((max-depth 0)
        (depth 0))
    (dolist (op (mapcar #'cdr (sort (mapcan #'span-points notes) #'span-point<)) max-depth)
      (cond ((eql op :start)
             (incf depth)
             (when (< max-depth depth)
               (setf max-depth depth)))
            ((eql op :end)
             (decf depth))))))

(defun offset-position (printer-state location)
  (gethash location (slot-value printer-state 'offset-positions) (cons 1 0)))

(defun note-highlight-char (note)
  (case (note-type note)
    (:primary #\^)
    (otherwise #\-)))

(defun write-char-n (char n stream)
  (dotimes (n n)
    (write-char char stream)))

;; Printer

(defun print-error-location (stream printer-state source-error)
  (with-slots (type source location message) source-error
    (destructuring-bind (line . column)
        (offset-position printer-state location)
      (format stream "~(~A~): ~A~%  --> ~A:~D:~D~%"
              type
              message
              (source-name source)
              line
              column))))

(defun print-line-prefix (stream printer-state &key (line-number nil))
  (with-slots (line-number-width) printer-state
    (cond (line-number
           (format stream " ~va |" line-number-width line-number))
          (t
           (write-char-n #\Space (+ 2 line-number-width) stream)
           (write-char #\| stream)))))

(defun print-line-number (stream printer-state line-number show-line-number)
  (with-slots (line-number-width note-stack) printer-state
    (print-line-prefix stream printer-state :line-number (and show-line-number line-number))
    (format stream
            " ~{~:[ ~;|~]~}"
            (mapcar
             (lambda (note)
               (>= (end-line printer-state note)
                   line-number))
             note-stack))))

(defun print-line-contents (stream printer-state line-number)
  (print-line-number stream printer-state line-number t)
  (with-slots (note-stack note-max-depth) printer-state
    (format stream "~v@{ ~}~A~%"
            (- note-max-depth (length note-stack))
            (line-contents printer-state line-number))))

(defun print-single-line-note (stream printer-state note)
  (multiple-value-bind (start-line start-column end-line end-column)
      (span-positions printer-state note)
    (declare (ignore end-line))
    (print-line-number stream printer-state start-line nil)
    (with-slots (note-max-depth note-stack) printer-state
      (format stream "~v{~C~:*~}~v{~C~:*~} ~A~%"
              (+ start-column
                 (- note-max-depth (length note-stack)))
              '(#\Space)
              (- end-column start-column)
              (list (note-highlight-char note))
              (note-message note)))))

(defun print-note-start (stream printer-state note)
  (destructuring-bind (start-line . start-column)
      (start-position printer-state note)
    (print-line-number stream printer-state start-line nil)
    (with-slots (note-max-depth note-stack) printer-state
      (write-char #\Space stream)
      (write-char-n #\_ (+ start-column (- note-max-depth (length note-stack) 1)) stream)
      (write-char (note-highlight-char note) stream)
      (terpri stream))))

(defun print-note-end (stream printer-state note)
  (let ((start-line (start-line printer-state note))
        (end-column (end-column printer-state note)))
    (print-line-number stream printer-state start-line nil)
    (with-slots (note-max-depth note-stack) printer-state
      (write-char-n #\_ (+ end-column (- note-max-depth (length note-stack) 1)) stream)
      (format stream "~C ~A~%" (note-highlight-char note) (note-message note)))))

(defun print-finished-notes-for-line (stream printer-state line-number)
  (with-slots (note-stack) printer-state
    ;; Check if there are any multiline notes that need to be printed
    (loop :for stack-head := note-stack :then (cdr stack-head)
          :for note := (car stack-head)
          :for end-line := (and note (end-line printer-state note))
          :when (null stack-head)
            :do (return)
          :when (= line-number end-line)
            :do (print-note-end stream printer-state note)
          :when (and (eq note (car note-stack))
                     (>= line-number end-line))
            :do (pop note-stack))))

(defun print-lines-until (stream printer-state line-number)
  (with-slots (current-line note-stack) printer-state
    (cond ((= line-number current-line)
           ;; If we are on the same line then don't reprint.
           )
          ((>= 3 (- line-number current-line))
           ;; If we are within 3 lines of the previous one then just
           ;; print those lines.
           (loop :for line :from current-line :below line-number
                 :do (print-line-contents stream printer-state (1+ line))
                 :unless (= (1+ line) line-number)
                   :do (print-finished-notes-for-line stream printer-state (1+ line))))
          (t
           ;; Otherwise split the output.
           (print-line-contents stream printer-state (1+ current-line))
           ;; Print out any intermediate multiline note endings.
           (loop :for note :in note-stack
                 :for end-line := (end-line printer-state note)
                 :when (< current-line end-line line-number)
                   :do (print-lines-until stream printer-state end-line))
           (format stream " ...~%")
           (print-line-contents stream printer-state (1- line-number))
           (print-line-contents stream printer-state line-number)))
    (setf current-line line-number)))

(defun print-note (stream printer-state note)
  (multiple-value-bind (start-line end-line)
      (span-lines printer-state note)
    (print-lines-until stream printer-state start-line)
    (cond ((/= start-line end-line)
           (print-note-start stream printer-state note)
           (push note (note-stack printer-state)))
          (t
           (print-single-line-note stream printer-state note)
           (print-finished-notes-for-line stream printer-state start-line)))))

(defun print-notes (stream printer-state)
  (with-slots (notes last-line) printer-state
    (loop :for note :in notes
          :do (print-note stream printer-state note))
    (print-lines-until stream printer-state last-line)
    (print-finished-notes-for-line stream printer-state last-line)))

(defun print-help (stream printer-state help)
  (with-slots (source-stream) printer-state
    (multiple-value-bind (start-line start-column end-line end-column)
        (span-positions printer-state help)
      (unless (= start-line end-line)
        (error "multiline help messages not supported"))
      (format stream "help: ~A~%" (note-message help))
      (let ((line (line-contents printer-state start-line)))
        (print-line-prefix stream printer-state :line-number start-line)
        (format stream " ~A" (subseq line 0 start-column))
        (let ((replaced-text (funcall (note-replacement help)
                                      (subseq line start-column end-column))))
          (format stream "~A~A~%" replaced-text (subseq line end-column))
          (print-line-prefix stream printer-state)
          (format stream "~v{~C~:*~}~v{~C~:*~}~%"
                  (1+ start-column) '(#\Space)
                  (length replaced-text) '(#\-)))))))

(defun print-empty-line (stream printer-state)
  (print-line-prefix stream printer-state)
  (terpri stream))

(defun make-printer-state (source-stream source-error)
  (with-slots (notes help context) source-error
    (make-instance 'printer-state
      :source-stream source-stream
      :notes (sort notes #'< :key #'span-start)
      :help help
      :context context)))

(defun print-source-error (stream source-error)
  (with-source-stream (source-stream source-error)
    (let ((state (make-printer-state source-stream source-error)))
      (print-error-location stream state source-error)
      (print-empty-line stream state)
      (with-slots (help context last-line) state
        (print-notes stream state)
        (loop :for help :in help
              :do (print-help stream state help))
        (loop :for context :in context
              :do (format stream "note: ~A~%" (error-context-message context)))))))

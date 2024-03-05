(defpackage #:source-error
  (:use
   #:cl)
  (:export
   #:context
   #:deferred-note
   #:help
   #:note
   #:print-source-error                 ; FUNCTION
   #:source-error                       ; MACRO, CONDITION
   #:source-error-info                  ; STRUCTURE
   #:source-location                    ; FUNCTION
   #:source-name                        ; GENERIC
   #:source-stream                      ; GENERIC
   #:source-warning))                   ; FUNCTION

(in-package #:source-error)

;; Source-aware error and warning condition types and helper functions
;;
;; These are the condition classes used by applications. Error and
;; warning conditions both have an `error` slot of type
;; `error-info` that contains detailed information about the
;; source of the error and optional explanatory notes and
;; messages. `print-source-error` uses this structure to emit an
;; annotated error description when either of these conditions is
;; printed.

;; source info protocol
;;
;; The generic functions `source-name` and `source-stream` define a
;; protocol that gives the condition printer access to an input source
;; related to the condition, via the `source` sl§>ot on a
;; error-info struct (see below). These provide, respectively,
;; the name, and contents of a source.

;; source annotation structures
;;
;; These are classes that hold for input span-associated error
;; annotations. The top-level `error-info` structure may
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

(define-condition source-error (error)
  ((error :initarg :error))
  (:documentation "The type for user-facing errors.")
  (:report report-error))

(define-condition source-warning (style-warning)
  ((error :initarg :error))
  (:documentation "The type for user-facing warnings.")
  (:report report-error))

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
    :type type
    :message message))

(defclass help (source-annotation)
  ((replacement :initarg :replacement
                :reader note-replacement)))

(defun help (&key span replacement message)
  (make-instance 'help
    :span span
    :replacement replacement
    :message message))

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

(defclass error-info ()
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
            :reader error-context)))

(defun positioned-annotations (error-info)
  (with-slots (notes help) error-info
    (concatenate 'list notes help)))

(defmethod initialize-instance :after ((error-info error-info) &rest initargs)
  (declare (ignore initargs))
  (with-slots (notes) error-info
    (setf notes (stable-sort notes #'< :key #'span-start))))

(defun source-error (&key (type :error) source location message primary-note notes help context)
  "Construct an ERROR-INFO instance with a message and primary note attached to the provided form.

MESSAGE and PRIMARY-NOTE must be supplied string arguments.
NOTES and HELP may optionally be supplied notes and help messages."
  (let ((span (etypecase location
                (cons location)
                (integer (cons (1- location) location)))))
    (make-instance 'error-info
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

(defmacro with-source-stream ((stream error-info) &body body)
  `(with-open-stream (,stream (source-stream (error-source ,error-info)))
    ,@body))

;; error formatter
;;
;; `print-source-error`, at the bottom, is a workhorse function that
;; takes a source-error-info structure, resolves source-relative spans
;; to the input source, and then prints annotated source code.
;;
;; `report-state` maintains state during printing: current line,
;; note depth, etc.

(defclass report-state ()
  ((source-stream :initarg :source-stream
                  :reader source-stream
                  :documentation "The stream form which source code is to be read.")
   (error-info :initarg :error-info)

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

(defmethod initialize-instance :after ((report-state report-state) &rest initargs)
  (declare (ignore initargs))
  (with-slots (source-stream
               error-info
               line-offsets
               offset-positions
               note-max-depth
               current-line
               last-line
               line-number-width)
      report-state
    (let* ((char-offsets (char-offsets report-state))
           (offsets (find-line-offsets source-stream (car (last char-offsets)))))
      (setf line-offsets (make-array (length offsets)
                                     :initial-contents offsets))
      (loop :for (char-offset line column) :in (find-column-offsets offsets char-offsets)
            :do (setf (gethash char-offset offset-positions)
                      (cons line column)))
      (setf current-line (first-line-number report-state)
            last-line (last-line-number report-state)
            line-number-width (line-number-width report-state)
            note-max-depth (max-depth (slot-value error-info 'notes))))))

(defun span-lines (report-state source-annotation)
  "Return the start and end lines of SOURCE-ANNOTATION"
  (with-slots (offset-positions) report-state
    (destructuring-bind (start . end) (span source-annotation)
      (values (car (gethash start offset-positions))
              (car (gethash end offset-positions))))))

(defun span-positions (report-state source-annotation)
  "Return the start and end positions of SOURCE-ANNOTATION"
  (with-slots (offset-positions) report-state
    (destructuring-bind (start . end) (span source-annotation)
      (destructuring-bind (start-line . start-column)
          (gethash start offset-positions)
        (destructuring-bind (end-line . end-column)
            (gethash end offset-positions)
          (values start-line start-column end-line end-column))))))

;; Mapping between character offsets and line and column positions.
;;
;; First line (zero indexed) = offset 0, etc.

(defun find-line-offsets (stream &optional limit)
  "Compute the offsets of lines in a stream."
  (file-position stream 0)
  (loop :with index := 0
        :for char := (read-char stream nil nil)
        :unless (and char (or (not limit)
                              (<= index limit)))
          :return (cons 0 offsets)
        :else
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

(defun line-contents (report-state line-number)
  (with-slots (line-offsets source-stream) report-state
    (let ((offset (if (= 1 line-number)
                      0
                      (aref line-offsets (1- line-number)))))
      (file-position source-stream offset)
      (read-line source-stream nil ""))))

(defun char-offsets (report-state)
  (sort (remove-duplicates
         (mapcan (lambda (note)
                   (list (span-start note) (span-end note)))
                 (positioned-annotations (slot-value report-state 'error-info))))
        #'<))

(defun first-line-number (report-state)
  (with-slots (error-info offset-positions) report-state
    (car (gethash (span-start (car (notes error-info))) offset-positions))))

(defun last-line-number (report-state)
  (with-slots (error-info offset-positions) report-state
    (car (gethash (span-end (car (last (notes error-info)))) offset-positions))))

(defun start-position (report-state span)
  (with-slots (offset-positions) report-state
    (gethash (span-start span) offset-positions)))

(defun start-line (report-state span)
  (with-slots (offset-positions) report-state
    (car (gethash (span-start span) offset-positions))))

(defun start-column (report-state span)
  (with-slots (offset-positions) report-state
    (cdr (gethash (span-start span) offset-positions))))

(defun end-position (report-state span)
  (with-slots (offset-positions) report-state
    (gethash (span-end span) offset-positions)))

(defun end-line (report-state span)
  (with-slots (offset-positions) report-state
    (car (gethash (span-end span) offset-positions))))

(defun end-column (report-state span)
  (with-slots (offset-positions) report-state
    (cdr (gethash (span-end span) offset-positions))))

(defun line-number-width (report-state)
  (1+ (floor (log (last-line-number report-state) 10))))

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

(defun offset-position (report-state location)
  (gethash location (slot-value report-state 'offset-positions) (cons 1 0)))

(defun note-highlight-char (note)
  (case (note-type note)
    (:primary #\^)
    (otherwise #\-)))

(defun write-char-n (char n stream)
  (dotimes (n n)
    (write-char char stream)))

;; Printer

(defun print-error-location (stream report-state)
  (with-slots (error-info) report-state
    (with-slots (type source location message) error-info
      (destructuring-bind (line . column)
          (offset-position report-state location)
        (format stream "~(~A~): ~A~%  --> ~A:~D:~D~%"
                type
                message
                (source-name source)
                line
                column)))))

(defun print-line-number (report-state line-number show-line-number)
  (with-slots (stream line-number-width note-stack) report-state
    (format stream
            " ~:[~vA~*~*~;~*~*~v{~C~:*~}~] | ~{~:[ ~;|~]~}"
            (not show-line-number)
            line-number-width
            line-number
            line-number-width
            '(#\Space)
            (mapcar
             (lambda (note)
               (>= (end-line report-state note)
                   line-number))
             note-stack))))

(defun print-line-contents (report-state line-number)
  (print-line-number report-state line-number t)
  (with-slots (stream file-stream note-stack note-max-depth) report-state
    (format stream " ~v@{ ~}~A~%"
            (- note-max-depth (length note-stack))
            (line-contents report-state line-number))))

(defun print-singleline-note (report-state note)
  (multiple-value-bind (start-line start-column end-line end-column)
      (span-positions report-state note)
    (declare (ignore end-line))
    (print-line-number report-state start-line nil)
    (with-slots (stream note-max-depth note-stack) report-state
      (format stream " ~v{~C~:*~}~v{~C~:*~} ~A~%"
              (+ start-column
                 (- note-max-depth (length note-stack)))
              '(#\Space)
              (- end-column start-column)
              (list (note-highlight-char note))
              (note-message note)))))

(defun print-note-start (report-state note)
  (destructuring-bind (start-line . start-column)
      (start-position report-state note)
    (print-line-number report-state start-line nil)
    (with-slots (stream note-max-depth note-stack) report-state
      (write-char-n #\_ (+ start-column (- note-max-depth (length note-stack))) stream)
      (write-char (note-highlight-char note) stream)
      (terpri stream))))

(defun print-note-end (report-state note)
  (let ((start-line (start-line report-state note))
        (end-column (end-column report-state note)))
    (print-line-number report-state start-line nil)
    (with-slots (stream note-max-depth note-stack) report-state
      (write-char-n #\_ (+ end-column (- note-max-depth (length note-stack))) stream)
      (format stream "~C ~A~%"
              (note-highlight-char note)
              (note-message note)))))

(defun print-finished-notes-for-line (report-state line-number)
  (with-slots (stream note-stack) report-state
    ;; Check if there are any multiline notes that need to be printed
    (loop :for stack-head := note-stack :then (cdr stack-head)
          :for note := (car stack-head)
          :for end-line := (and note (end-line report-state note))
          :when (null stack-head)
            :do (return)
          :when (= line-number end-line)
            :do (print-note-end report-state note)
          :when (and (eq note (car note-stack))
                     (>= line-number end-line))
            :do (pop note-stack))))

(defun print-lines-until (report-state line-number)
  (with-slots (stream current-line note-stack) report-state
    (cond ((= line-number current-line)
           ;; If we are on the same line then don't reprint.
           )
          ((>= 3 (- line-number current-line))
           ;; If we are within 3 lines of the previous one then just
           ;; print those lines.
           (loop :for line :from current-line :below line-number
                 :do (print-line-contents report-state (1+ line))
                 :unless (= (1+ line) line-number)
                   :do (print-finished-notes-for-line report-state (1+ line))))
          (t
           ;; Otherwise split the output.
           (print-line-contents report-state (1+ current-line))
           ;; Print out any intermediate multiline note endings.
           (loop :for note :in note-stack
                 :for end-line := (end-line report-state note)
                 :when (< current-line end-line line-number)
                   :do (print-lines-until report-state end-line))
           (format stream " ...~%")
           (print-line-contents report-state (1- line-number))
           (print-line-contents report-state line-number)))
    (setf current-line line-number)))

(defun print-note (report-state note)
  (multiple-value-bind (start-line end-line) (span-lines report-state note)
    (print-lines-until report-state start-line)
    (cond ((/= start-line end-line)
           (print-note-start report-state note)
           (push note (note-stack report-state)))
          (t
           (print-singleline-note report-state note)
           (print-finished-notes-for-line report-state start-line)))))

(defun print-help (stream report-state help)
  (with-slots (source-stream) report-state
    (multiple-value-bind (start-line start-column end-line end-column)
        (span-positions report-state help)
      (unless (= start-line end-line)
        (error "multiline help messages not supported yet."))
      (format stream "help: ~A~%" (note-message help))
      (let ((line (line-contents report-state start-line)))
        (print-line-prefix stream report-state :line-number start-line)
        (format stream " ~A" (subseq line 0 start-column))
        (let ((replaced-text (funcall (note-replacement help)
                                      (subseq line start-column end-column))))
          (format stream "~A~A~%" replaced-text (subseq line end-column))
          (print-line-prefix stream report-state)
          (format stream "~v{~C~:*~}~v{~C~:*~}~%"
                  (1+ start-column) '(#\Space)
                  (length replaced-text) '(#\-)))))))

(defun print-line-prefix (stream report-state &key (line-number nil))
  (with-slots (line-number-width) report-state
    (cond (line-number
           (format stream " ~va |" line-number-width line-number))
          (t
           (write-char-n #\Space (+ 2 line-number-width) stream)
           (write-char #\| stream)))))

(defun print-empty-line (stream report-state)
  (print-line-prefix stream report-state)
  (terpri stream))

(defun can-advance-p (report-state)
  (with-slots (current-line last-line) report-state
    (<= current-line last-line)))

(defun advance (stream report-state)
  (declare (ignore stream))
  (incf (slot-value report-state 'current-line))

        ;; Print notes, printing lines as needed and keeping track of
        ;; multiline note depth
;        (loop :for note :in (notes error-info)
 ;             :do (print-note state note))
  ;      (print-lines-until state last-line)
   ;     (print-finished-notes-for-line state last-line)

  )

(defun print-source-error (stream error-info)
  (with-source-stream (source-stream error-info)
    (let ((state (make-instance 'report-state
                   :source-stream source-stream
                   :error-info error-info)))

      ;; error message and location
      
      (print-error-location stream state)
      (print-empty-line stream state)
      
      (loop :while (can-advance-p state)
            :do (advance stream state))
      
      (loop :for help :in (slot-value error-info 'help)
            :do (print-help stream state help))
      
      #+yow
      (loop :for context :in (slot-value error-info 'error-context)
            :do (format stream "note: ~A~%" (error-context-message context))))))

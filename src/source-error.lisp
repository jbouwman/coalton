(defpackage #:source-error
  (:use
   #:cl)
  (:export
   #:context
   #:deferred-note
   #:help
   #:note
   #:report-source-error                ; FUNCTION
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
;; messages. `report-source-error` uses this structure to emit an
;; annotated error description when either of these conditions is
;; printed.

;; source info protocol
;;
;; The generic functions `source-name` and `source-stream` define a
;; protocol that gives the condition printer access to an input source
;; related to the condition, via the `source` slot on a
;; error-info struct (see below). These provide, respectively,
;; the name, and contents of a source.

;; source annotation structures
;;
;; These are classes that hold for input span-associated error
;; annotations. The top-level `error-info` structure may
;; contain sets of optional `note` and
;; `source-error-help` messages that refer to source spans, which
;; `report-source-error` will resolve to character ranges in an input
;; source.

(defun report-error (condition stream)
  (with-slots (error) condition
    (report-source-error stream error)))

(define-condition source-error (error)
  ((error :initarg :error))
  (:documentation "The type for user-facing errors.")
  (:report report-error))

(define-condition source-warning (style-warning)
  ((error :initarg :error))
  (:documentation "The type for user-facing warnings.")
  (:report report-error))

(defclass span-mixin ()
  ((span :initarg :span
         :reader span)))

(defun span-start (span-mixin)
  (car (slot-value span-mixin 'span)))

(defun span-end (span-mixin)
  (cdr (slot-value span-mixin 'span)))

(defclass note (span-mixin)
  ((type :initarg :type
         :reader note-type)
   (message :initarg :message
            :reader note-message)))

(defclass help (span-mixin)
  ((replacement :initarg :replacement
                :reader note-replacement)
   (message :initarg :message
            :reader note-message)))

(defclass deferred-note ()
  ((fn :initarg :fn)
   (rendered)))

(defclass error-context ()
  ((message :initarg :message
            :reader error-context-message)))

(defclass error-info ()
  ((type :initarg :type)
   (source :initarg :source
           :reader error-source)
   (location :initarg :location)
   (message :initarg :message)
   (notes :initarg :notes
          :reader notes)
   (help :initarg :help)
   (context :initarg :context
            :reader error-context)))

(defmethod initialize-instance :after ((error-info error-info) &rest initargs)
  (declare (ignore initargs))
  (with-slots (notes) error-info
    (setf notes (stable-sort notes #'< :key #'span-start))))

(defun note (&key span type message)
  (make-instance 'note
    :span span
    :type type
    :message message))

(defun help (&key span replacement message)
  (make-instance 'help
    :span span
    :replacement replacement
    :message message))

(defun deferred-note (fn)
  (make-instance 'deferred-note :fn fn))

(defun make-deferred-error-note (fn)
  (make-instance 'deferred-note :fn fn))

(defun context (&key message)
  (make-instance 'error-context :message message))

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

(defgeneric source-name (source)
  (:documentation "Returns a string that names a source, for reporting in conditions. In
the case of files, this is the filename path."))

(defgeneric source-stream (source)
  (:documentation "A seekable stream providing access to the original contents of a
source, for reporting in condition printers. WARNING: it is the
responsibility of the caller to close the stream, if not using the
with-source-stream convenience macro."))

(defmacro with-source-stream ((stream error-info) &body body)
  `(with-open-stream (,stream (source-stream (error-source ,error-info)))
    ,@body))

;; error formatter
;;
;; `report-source-error`, at the bottom, is a workhorse function that
;; takes a source-error-info structure, resolves source-relative spans
;; to the input source, and then prints annotated source code.
;;
;; `report-state` maintains state during printing: current line,
;; note depth, etc.

(defclass report-state ()
  ((stream :initarg :stream
           :documentation "The stream onto which the condition is being written.")
   (source-stream :initarg :source-stream
                  :reader source-stream
                  :documentation "The stream form which source code is to be read.")
   (error-info :initarg :error-info)

   (line-offsets :reader line-offsets)
   (offset-positions :initform (make-hash-table))

   (line-number-width)
   (first-line)
   (current-line)
   (last-line)

   ;; We need to keep track of the current depth of multiline
   ;; notes so that we can pad the left with the correct
   ;; number of columns.

   (note-stack :initform nil)
   (note-max-depth :initform 0)))

(defmethod initialize-instance :after ((report-state report-state) &rest initargs)
  (declare (ignore initargs))
  (with-slots (source-stream
               error-info
               line-offsets
               offset-positions
               note-max-depth
               first-line
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
      (setf last-line (last-line-number report-state)
            line-number-width (line-number-width report-state)
            note-max-depth (max-depth (slot-value error-info 'notes))))))

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
          :return offsets
        :else
          :when (char= char #\Newline)
            :collect (1+ index) :into offsets
        :do (incf index)))

(defun find-column-offsets (line-offsets offsets)
  "Given the offsets of newlines in a stream, compute the line and
column numbers for a sequence of absolute stream offsets."
  (loop :with line := 1
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
    (let ((offset (aref line-offsets (1- line-number))))
      (file-position source-stream offset)
      (read-line source-stream))))

(defun char-offsets (report-state)
  (sort (remove-duplicates
         (mapcan (lambda (note)
                   (list (span-start note) (span-end note)))
                 (notes (slot-value report-state 'error-info))))
        #'<))

(defun first-line-number (report-state)
  (with-slots (error-info offset-positions) report-state
    (car (gethash (span-start (car (notes error-info))) offset-positions))))

(defun last-line-number (report-state)
  (with-slots (error-info offset-positions) report-state
    (car (gethash (span-end (car (last (notes error-info)))) offset-positions))))

(defun span-lines (report-state span)
  (with-slots (offset-positions) report-state
    (values (car (gethash (span-start span) offset-positions))
            (car (gethash (span-end span) offset-positions)))))

(defun span-offsets (report-state span)
  (with-slots (offset-positions) report-state
    (cons (gethash (span-start span) offset-positions)
          (gethash (span-end span) offset-positions))))

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

(defun print-error-location (report-state)
  (with-slots (error-info stream) report-state
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
            (- note-max-depth
               (length note-stack))
            (line-contents report-state line-number))))

(defun note-highlight-char (note)
  (case (note-type note)
    (:primary #\^)
    (otherwise #\-)))

(defun print-singleline-note (report-state note)
  (destructuring-bind ((start-line . start-column)
                       (end-line . end-column))
      (span-offsets report-state (span note))
    (declare (ignore end-line))
    (print-line-number report-state start-line nil)
    (with-slots (stream note-max-depth note-stack) report-state
      (format stream
              " ~v{~C~:*~}~v{~C~:*~} ~A~%"
              (+ start-column
                 (- note-max-depth
                    (length note-stack)))
              '(#\Space)
              (- end-column start-column)
              (list (note-highlight-char note))
              (note-message note)))))

(defun write-char-n (char n stream)
  (dotimes (n n)
    (write-char char stream)))

(defun print-multiline-note-start (report-state note)
  (destructuring-bind (start-line . start-column)
      (start-position report-state (span note))
    (print-line-number report-state start-line nil)
    (with-slots (stream note-max-depth note-stack) report-state
      (write-char-n #\_ (+ start-column (- note-max-depth (length note-stack))) stream)
      (write-char (note-highlight-char note) stream))))

(defun print-note-end (report-state note)
  (let ((start-line (start-line report-state (span note)))
        (end-column (end-column report-state (span note))))
    (print-line-number report-state start-line nil)
    (with-slots (stream note-max-depth note-stack) report-state
      (write-char-n #\_ (+ end-column (- note-max-depth (length note-stack))) stream)
      (format stream "~C ~A~%"
              (note-highlight-char note)
              (note-message note)))))

(defun print-finished-multiline-notes-for-line (report-state line-number)
  (with-slots (stream note-stack) report-state
    ;; Check if there are any multiline notes that need to be printed
    (loop :for stack-head := note-stack :then (cdr stack-head)
          :for note := (car stack-head)
          :for end-line := (end-line report-state (span note))
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
                   :do (print-finished-multiline-notes-for-line report-state (1+ line))))
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
  (with-slots (note-stack note-max-depth) report-state
    (multiple-value-bind (start-line end-line)
        (span-lines report-state (span note))
      ;; Print lines until this note.
      (print-lines-until report-state start-line)
      (cond
        ;; For multiline we need to add to the current stack.
        ((/= start-line end-line)
         ;; Print out a new row with underlines connecting
         ;; back to the multiline position.
         (print-multiline-note-start report-state note)
         ;; Push this note on to the stack for safe keeping.
         (push note note-stack))
        ;; For non-multiline just print the note
        (t
         (print-singleline-note report-state note)
         (print-finished-multiline-notes-for-line report-state start-line))))))

(defun print-help (report-state help)
  (with-slots (stream source-stream) report-state
    (destructuring-bind ((start-line . start-line-start)
                         (end-line . end-line-start))
        (span-offsets report-state (span help))
      (unless (= start-line end-line)
        (error "multiline help messages not supported yet."))
      (let ((line-number-width (1+ (floor (log end-line 10))))
            (start (span-start (span help)))
            (end (span-end (span help))))
        (format stream "help: ~A~%" (note-message help))
        (format stream " ~vD | ~A"
                line-number-width
                start-line
                (subseq (line-contents source-stream start-line)
                        0 (- start start-line-start)))
        (let ((replaced-text (funcall (note-replacement help)
                                      (subseq (line-contents source-stream start-line)
                                              (- start start-line-start)
                                              (- end end-line-start)))))
          (format stream "~A~A~%"
                  replaced-text
                  (subseq (line-contents source-stream start-line)
                          (- end end-line-start)))
          (format stream " ~v{~C~:*~} |~v{~C~:*~}~v{~C~:*~}~%"
                  line-number-width '(#\Space)
                  (1+ (- start start-line-start)) '(#\Space)
                  (length replaced-text) '(#\-)))))))

(defun report-source-error (stream error-info)
  (with-source-stream (source-stream error-info)
    (let ((state (make-instance 'report-state
                   :stream stream
                   :source-stream source-stream
                   :error-info error-info)))
      (with-slots (line-number-width last-line) state
        ;; Print the error message and location
        (print-error-location state)
        ;; Print first empty line.
        (format stream " ~va |~%" line-number-width "")
        ;; Print notes, printing lines as needed and keeping track of
        ;; multiline note depth
;        (loop :for note :in (slot-value error-info 'notes)
;              :do (print-note state note))
;        (print-lines-until state last-line)
;        (print-finished-multiline-notes-for-line state last-line)
;        (loop :for help :in (slot-value error-info 'help)
;              :do (print-help state help))
;        (loop :for context :in (slot-value error-info 'error-context)
                                        ;              :do (format stream "note: ~A~%" (error-context-message context)))
        ))))

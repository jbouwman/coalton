;;;; Source and location-aware condition classes, for reporting syntax
;;;; and other compiler errors, with complex references to multiple
;;;; locations.

(defpackage #:source-error/error
  (:use
   #:cl)
  (:export
   #:char-position-stream
   #:find-line-offsets
   #:report-source-condition
   #:location
   #:docstring
   #:primary-p
   #:help-p
   #:message
   #:replace-function
   #:source
   #:span
   #:source-available-p
   #:source-condition
   #:source-condition-message
   #:source-error
   #:source-name
   #:source-stream
   #:source-warning
   #:with-context
   #:with-note))

(in-package #:source-error/error)

;;; source protocol

(defgeneric source-stream (source)
  (:documentation "Open and return a stream from which source text may be read. The caller is responsible for closing the stream, and the stream's initial position may be greater than zero."))

(defgeneric source-name (source)
  (:documentation "The name of an error's source, suitable for reporting in errors. If the source is a file, SOURCE-NAME will be that file's absolute path."))

(defgeneric source-available-p (source)
  (:documentation "T if source-stream is available."))

;;; location protocol

(defgeneric location (something))

(defgeneric source (location))

(defgeneric span (location))

(deftype span ()
  '(cons fixnum fixnum))

(defun span-start (span)
  (car span))

(defun span-end (span)
  (cdr span))

;;; source conditions

(defvar *context* nil)

(defmacro with-context ((key message) &body body)
  `(let ((*context* (cons (cons ,key ,message) *context*)))
     ,@body))

(define-condition source-condition ()
  ((message :initarg :message
            :reader source-condition-message)
   (notes :initarg :notes
          :reader source-condition-notes)
   (context :initform *context*
            :reader source-condition-context))
  (:report report-source-condition))

(defgeneric severity (condition))

(define-condition source-error (source-condition error)
  ()
  (:documentation "A user-facing error."))

(defmethod severity ((condition source-error))
  :error)

(define-condition source-warning (source-condition style-warning)
  ()
  (:documentation "A user-facing warning."))

(defmethod severity ((condition source-warning))
  :warn)

(defun source-error (message &rest notes)
  (error 'source-error
         :message message
         :notes notes))

(defun source-warning (message &rest notes)
  (error 'source-warning
         :message message
         :notes notes))

(defgeneric message (object)
  (:documentation "The primary message associated with an object."))

(defgeneric primary-p (note))

(defgeneric help-p (note))

(defgeneric replace-function (note))

;;; Printer
;;;
;;; `report-source-condition`, at the bottom, is a workhorse function that
;;; takes a source condition, resolves source-relative locations
;;; to the input source, and prints annotated source code.
;;;
;;; `printer-state` maintains state during printing: current line,
;;; note depth, etc.

(defclass printer-state ()
  ((source-stream :initarg :source-stream
                  :initform nil
                  :reader source-stream)
   (notes :initarg :notes
          :initform nil)
   (help :initarg :help
         :initform nil)
   (context :initarg :context
            :initform nil)
   (line-offsets :reader line-offsets)
   (offset-positions :initform (make-hash-table))
   (line-number-width :initform 0
                      :accessor line-number-width)
   (current-line :initform 0
                 :accessor current-line)
   (last-line :initform 0
              :accessor last-line)
   ;; track the current depth of multiline notes in order to pad
   ;; the left margin with the correct number of columns.
   (note-stack :accessor note-stack
               :initform nil)
   (note-max-depth :initform 0
                   :accessor note-max-depth)))

(defun %reduce-context (context)
  (mapcar #'cdr
          (reduce (lambda (acc context)
                    (if (assoc (car context) acc)
                        acc
                        (cons context acc)))
                  context
                  :initial-value nil)))

(defun make-printer-state (source-stream notes context)
  (make-instance 'printer-state
    :source-stream source-stream
    :notes (remove-if #'help-p notes)   ; FIXME merge these
    :help (remove-if-not #'help-p notes)   ; FIXME merge these
    :context (%reduce-context context)))

(defun first-line-number (notes offset-positions)
  (when notes
    (span-start (gethash (start-offset (car notes)) offset-positions))))

(defun last-line-number (notes offset-positions)
  (when notes
    (let ((last-offset (apply #'max (mapcar #'end-offset notes))))
      (span-start (gethash last-offset offset-positions)))))

(defmethod initialize-instance :after ((printer-state printer-state) &rest initargs)
  (declare (ignore initargs))
  (with-slots (source-stream            ; FIXME with-slots
               line-offsets
               offset-positions
               notes)
      printer-state
    (when source-stream
      (let ((char-offsets (char-offsets printer-state))
            (offsets (find-line-offsets source-stream)))
        (setf line-offsets
              (coerce offsets 'vector))
        (setf (last-line printer-state)
              (length line-offsets))
        (loop :for (char-offset line column) :in (find-column-offsets offsets char-offsets)
              :do (setf (gethash char-offset offset-positions)
                        (cons line column)))
        (when notes
          (setf (current-line printer-state)
                (1- (first-line-number notes offset-positions))
                (last-line printer-state)
                (last-line-number notes offset-positions)))
        (setf (line-number-width printer-state) (1+ (floor (log (last-line printer-state) 10)))
              (note-max-depth printer-state) (max-depth offset-positions notes))))))

(defun positioned-annotations (printer-state)
  (with-slots (notes help) printer-state
    (concatenate 'list notes help)))

(defun char-offsets (printer-state)
  (sort (remove-duplicates
         (mapcan (lambda (note)
                   (list (start-offset note) (end-offset note)))
                 (positioned-annotations printer-state)))
        #'<))

(defun start-offset (note)
  (span-start (span (location note))))

(defun start-position (printer-state note)
  (with-slots (offset-positions) printer-state
    (gethash (start-offset note) offset-positions)))

(defun end-offset (note)
  (span-end (span (location note))))

(defun end-position (printer-state note)
  (with-slots (offset-positions) printer-state
    (gethash (end-offset note) offset-positions)))

(defun location-lines (printer-state note)
  "Return the start and end lines of NOTE"
  (let ((start-line (car (start-position printer-state note)))
        (end-line (car (end-position printer-state note))))
    (values start-line end-line)))

(defun location-positions (printer-state note)
  "Return the start and end positions of NOTE"
  (with-slots (offset-positions) printer-state
    (destructuring-bind (start-line . start-column)
        (gethash (start-offset note) offset-positions)
      (destructuring-bind (end-line . end-column)
          (gethash (end-offset note) offset-positions)
        (values start-line start-column end-line end-column)))))

;; Mapping between character offsets and line and column positions.
;;
;; First line (zero indexed) = offset 0, etc.

(defun find-line-offsets (stream)
  "Compute the offsets of lines in STREAM."
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
    (let ((offset (aref line-offsets (1- line-number))))
      (file-position source-stream offset)
      (read-line source-stream nil ""))))

(defun start-line (printer-state location)
  (with-slots (offset-positions) printer-state
    (car (gethash (start-offset location) offset-positions))))

(defun start-column (printer-state location)
  (with-slots (offset-positions) printer-state
    (cdr (gethash (start-offset location) offset-positions))))

(defun end-line (printer-state location)
  (with-slots (offset-positions) printer-state
    (car (gethash (end-offset location) offset-positions))))

(defun end-column (printer-state location)
  (with-slots (offset-positions) printer-state
    (cdr (gethash (end-offset location) offset-positions))))

(defun location-point< (a b)
  (destructuring-bind (offset-a . type-a) a
    (destructuring-bind (offset-b . type-b) b
      (if (= offset-a offset-b)
          (and (eql type-a :start)
               (eql type-b :end))
          (< offset-a offset-b)))))

(defun location-points (location)
  (list (cons (start-offset location) :start)
        (cons (end-offset location) :end)))

(defun single-line-p (offset-positions note)
  (= (car (gethash (start-offset note) offset-positions))
     (car (gethash (end-offset note) offset-positions))))

(defun max-depth (offset-positions notes)
  (let ((max-depth 0)
        (depth 0)
        (multiline (remove-if (lambda (note)
                                (single-line-p offset-positions note))
                              notes)))
    (dolist (op (mapcar #'cdr (sort (mapcan #'location-points multiline) #'location-point<)) max-depth)
      (cond ((eql op :start)
             (incf depth)
             (when (< max-depth depth)
               (setf max-depth depth)))
            ((eql op :end)
             (decf depth))))))

(defun offset-position (printer-state location)
  (let ((location (etypecase location
                    (cons (car location))
                    (integer location))))
    (gethash location (slot-value printer-state 'offset-positions) (cons 1 0))))

(defun note-highlight-char (note)
  (if (primary-p note)
      #\^
      #\-))

(defun write-nchar (char n stream)
  (dotimes (n n)
    (write-char char stream)))

;;; Printer

;;; A line is composed of 3 sections
;;;
;;; 1. prefix  line number (if warranted) and a padded vertical column separator
;;; 2. margin  vertical arrow components for multiline notes. If there are no multiline notes, this column is zero-width.
;;; 3. content source text, or multiline arrow start and end points
;;;
;;; 1    2 3
;;; -----__---- ...
;;;
;;;  3 |   (define-type BigType)
;;;    |   ^^^^^^^^^^^^^^^^^^^^^ first definition here
;;;  4 |
;;;  5 |   (define-struct BigType
;;;    |  _^
;;;  6 | |   (wow String))
;;;    | |_______________^ second definition here

(defun print-prefix (stream printer-state &optional line-number)
  "Print the line number column, with optional LINE-NUMBER."
  (format stream " ~VA |"
          (line-number-width printer-state)
          (or line-number #\Space)))

(defun print-margin (stream printer-state &optional start-end)
  "Print the margin column, for vartical arrows connecting multiline notes."
  (let ((max-depth (note-max-depth printer-state)))
    (when (< 0 max-depth)
      (write-char #\Space stream)
      (let ((depth (length (note-stack printer-state))))
        (write-nchar #\| depth stream)
        (cond (start-end
               (write-char start-end stream)
               (write-nchar #\_ (- max-depth depth) stream))
              (t
               (write-nchar #\Space (- max-depth depth) stream)))))))

(defun print-line (stream printer-state line-number)
  (print-prefix stream printer-state line-number)
  (print-margin stream printer-state)
  (format stream " ~A~%" (line-contents printer-state line-number)))

(defun print-single-line-note (stream printer-state note)
  (multiple-value-bind (start-line start-column end-line end-column)
      (location-positions printer-state note)
    (declare (ignore start-line end-line))
    (print-prefix stream printer-state)
    (print-margin stream printer-state)
    (write-nchar #\Space (1+ start-column) stream)
    (write-nchar (note-highlight-char note) (max 1 (- end-column start-column)) stream)
    (format stream " ~A~%" (message note))))

(defun print-note-start (stream printer-state note)
  (print-prefix stream printer-state)
  (print-margin stream printer-state #\Space)
  (write-nchar #\_ (span-end (start-position printer-state note)) stream)
  (format stream "~C~%" (note-highlight-char note)))

(defun print-note-end (stream printer-state note)
  (print-prefix stream printer-state)
  (print-margin stream printer-state #\_)
  (write-nchar #\_ (1- (end-column printer-state note)) stream)
  (format stream "~C ~A~%" (note-highlight-char note) (message note)))

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
                 :do (print-line stream printer-state (1+ line))
                 :unless (= (1+ line) line-number)
                   :do (print-finished-notes-for-line stream printer-state (1+ line))))
          (t
           ;; Otherwise split the output.
           (print-line stream printer-state (1+ current-line))
           ;; Print out any intermediate multiline note endings.
           (loop :for note :in note-stack
                 :for end-line := (end-line printer-state note)
                 :when (< current-line end-line line-number)
                   :do (print-lines-until stream printer-state end-line))
           (format stream " ...~%")
           (print-line stream printer-state (1- line-number))
           (print-line stream printer-state line-number)))
    (setf current-line line-number)))

(defun print-note (stream printer-state note)
  (multiple-value-bind (start-line end-line)
      (location-lines printer-state note)
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
        (location-positions printer-state help)
      (unless (= start-line end-line)
        (error "multiline help messages not supported"))
      (format stream "help: ~A~%" (message help))
      (let ((line (line-contents printer-state start-line)))
        (print-prefix stream printer-state start-line)
        (format stream " ~A" (subseq line 0 start-column))
        (let ((replaced-text (funcall (or (replace-function help) #'identity)
                                      (subseq line start-column end-column))))
          (format stream "~A~A~%" replaced-text (subseq line end-column))
          (print-prefix stream printer-state)
          (format stream "~v{~C~:*~}~v{~C~:*~}~%"
                  (1+ start-column) '(#\Space)
                  (length replaced-text) '(#\-)))))))

(defun print-state (stream state)
  (with-slots (help context last-line) state
    (print-notes stream state)
    (loop :for help :in help
          :do (print-help stream state help))
    (loop :for context :in context
          :do (format stream "note: ~A~%" context))))

(defun print-condition-header (condition stream source note &optional state)
  (format stream "~(~A~): ~A~%  --> ~A"
          (severity condition)
          (source-condition-message condition)
          (source-name source))
  (cond ((not state)
         (write-string " (source unavailable)" stream)
         (terpri stream))
        (t
         (destructuring-bind (line . column)
             (offset-position state (start-offset note))
           (format stream ":~D:~D~%" line column)))))

(defun report-source-condition (condition stream)
  (let* ((primary-note (first (source-condition-notes condition)))
         (source (source (location primary-note))))
    (unless (source-available-p source)
      (print-condition-header condition stream source primary-note)
      (return-from report-source-condition (values)))
    (with-open-stream (source-stream (source-stream source))
      (let ((state (make-printer-state source-stream
                                       (sort (copy-list (source-condition-notes condition)) #'< :key #'start-offset)
                                       (source-condition-context condition))))
        (print-condition-header condition stream source primary-note state)
        (print-prefix stream state)
        (terpri stream)
        (print-state stream state)))))

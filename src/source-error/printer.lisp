(in-package #:source-error)

;; error formatter
;;
;; `print-source-error`, at the bottom, is a workhorse function that
;; takes a source-error-info structure, resolves source-relative spans
;; to the input source, and then prints annotated source code.
;;
;; Other types here:
;;
;; `source-error-resolved-note` represents resolved notes, where 'resolved' means
;; that raw offsets into the source text have matched to line numbers.
;;
;; `printer-state` maintains state during printing: current line,
;; note depth, etc.
;;
;; Most functions with the prefix 'print-' are helpers for the main
;; 'print-source-error' function, and accept printer-state as first
;; argument.

(defclass printer-state ()
  ((stream :initarg :stream
           :documentation "The stream onto which the condition is being written.")
   (source-stream :initarg :source-stream
                  :documentation "The stream form which source code is to be read.")
   (error-info :initarg :error-info)

   (line-offsets)
   (offset-positions :initform (make-hash-table))

   (line-number-width)
   (first-line)
   (current-line)
   (last-line)

   ;; We need to keep track of the current depth of multiline
   ;; notes so that we can pad the left with the correct
   ;; number of columns.

   (note-stack :initform nil)
   (note-current-depth :initform 0)
   (note-max-depth :initform 0)))

(defun char-offsets (printer-state)
  (sort (remove-duplicates
         (mapcan (lambda (note)
                   (list (span-start note) (span-end note)))
                 (notes (slot-value printer-state 'error-info))))
        #'<))

(defun first-line-number (printer-state)
  (with-slots (error-info offset-positions) printer-state
    (car (gethash (span-start (car (notes error-info))) offset-positions))))

(defun last-line-number (printer-state)
  (with-slots (error-info offset-positions) printer-state
    (car (gethash (span-end (car (last (notes error-info)))) offset-positions))))

(defun line-number-width (printer-state)
  (1+ (floor (log (last-line-number printer-state) 10))))

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

;; Do all important precalculations here: with of line number column, max depth of notes

(defmethod initialize-instance :after ((printer-state printer-state) &rest initargs)
  (declare (ignore initargs))
  (with-slots (source-stream
               error-info
               line-offsets
               offset-positions
               note-max-depth
               first-line
               last-line
               line-number-width)
      printer-state
    (let ((char-offsets (char-offsets printer-state)))
      (setf line-offsets (line-offsets source-stream (car (last char-offsets))))
      (loop :for (char-offset line column) :in (resolve-offsets line-offsets char-offsets)
            :do (setf (gethash char-offset offset-positions)
                      (cons line column)))
      (setf first-line (first-line-number printer-state)
            last-line (last-line-number printer-state)
            line-number-width (line-number-width printer-state)
            note-max-depth (max-depth (slot-value error-info 'notes))))))

(defun offset-position (printer-state location)
  (gethash location (slot-value printer-state 'offset-positions) (cons 1 0)))

(defun print-error-location (printer-state)
  (with-slots (error-info stream) printer-state
    (with-slots (type source location message) error-info
      (destructuring-bind (line . column)
          (offset-position printer-state location)
        (format stream "~(~A~): ~A~%  --> ~A:~D:~D~%"
                type
                message
                (source-name source)
                line
                column)))))

(defun print-line-number (printer-state line-number show-line-number)
  (with-slots (stream line-number-width note-stack) printer-state
    (format stream
            " ~:[~vA~*~*~;~*~*~v{~C~:*~}~] | ~{~:[ ~;|~]~}"
            (not show-line-number)
            line-number-width
            line-number
            line-number-width
            '(#\Space)
            (mapcar
             (lambda (note)
               (>= (source-error-resolved-note-end-line note)
                   line-number))
             note-stack))))

(defun print-line-contents (printer-state line-number)
  (print-line-number printer-state line-number t)
  (with-slots (stream file-stream note-max-depth note-current-depth) printer-state
    (format stream " ~v@{ ~}~A~%"
            (- note-max-depth
               note-current-depth)
            (get-nth-line file-stream line-number))))

(defun note-highlight-char (note)
  (case (note-type note)
    (:primary #\^)
    (otherwise #\-)))

(defun print-singleline-note (printer-state note)
  (print-line-number printer-state (source-error-resolved-note-start-line note) nil)
  (with-slots (stream note-max-depth note-current-depth) printer-state
    (format stream
            " ~v{~C~:*~}~v{~C~:*~} ~A~%"
            (+ (source-error-resolved-note-start-column note)
               (- note-max-depth
                  note-current-depth))
            '(#\Space)
            (- (source-error-resolved-note-end-column note)
               (source-error-resolved-note-start-column note))
            (list (note-highlight-char note))
            (source-error-resolved-note-message note))))

(defun print-multiline-note-start (printer-state note)
  (print-line-number printer-state (source-error-resolved-note-start-line note) nil)
  (with-slots (stream note-max-depth note-current-depth) printer-state
    (format stream " ~v{~C~:*~}~C~%"
            (+ (source-error-resolved-note-start-column note)
               (- note-max-depth
                  note-current-depth))
            '(#\_)
            (note-highlight-char note))))

(defun print-multiline-note-end (printer-state note)
  (print-line-number printer-state (source-error-resolved-note-start-line note) nil)
  (with-slots (stream note-max-depth note-current-depth) printer-state
    (format stream
            "~v{~C~:*~}~C ~A~%"
            (+ (source-error-resolved-note-end-column note)
               (- note-max-depth
                  note-current-depth))
            '(#\_)
            (note-highlight-char note)
            (source-error-resolved-note-message note))))

(defun print-finished-multiline-notes-for-line (printer-state line-number)
  (with-slots (stream note-stack note-current-depth) printer-state
    ;; Check if there are any multiline notes that need to be printed
    (loop :for stack-head := note-stack :then (cdr stack-head)
          :for note := (car stack-head)
          :when (null stack-head)
            :do (return)
          :when (= line-number (source-error-resolved-note-end-line note))
            :do (print-note-end printer-state note)
          :when (and (eq note (car note-stack))
                     (>= line-number (source-error-resolved-note-end-line note)))
            :do (pop note-stack)
                (decf note-current-depth))))

(defun print-lines-until (printer-state line-number)
  (with-slots (stream current-line note-stack) printer-state
    (cond ((= line-number current-line)
           ;; If we are on the same line then
           ;; don't reprint.
           )
          ((>= 3 (- line-number current-line))
           ;; If we are within 3 lines of the
           ;; previous one then just print those
           ;; lines.
           (loop :for line :from current-line :below line-number
                 :do (print-line-contents printer-state (1+ line))
                 :unless (= (1+ line) line-number)
                   :do (print-finished-multiline-notes-for-line printer-state (1+ line))))
          (t
           ;; Otherwise split the output.
           (print-line-contents printer-state (1+ current-line))
           ;; Print out any intermediate multiline note endings.
           (loop :for note :in note-stack
                 :when (< current-line (source-error-resolved-note-end-line note) line-number)
                   :do (print-lines-until printer-state (source-error-resolved-note-end-line note)))
           (format stream " ...~%")
           (print-line-contents printer-state (1- line-number))
           (print-line-contents printer-state line-number)))
    (setf current-line line-number)))

(defun print-note (printer-state note)
  (with-slots (note-stack note-current-depth note-max-depth) printer-state
    (cond

      ;; todo lookup line/col position in offset cache
      
      ;; For multiline we need to add to the current stack.
      ((/= (source-error-resolved-note-start-line note)
           (source-error-resolved-note-end-line note))
       ;; Print lines until this note.
       (print-lines-until printer-state (source-error-resolved-note-start-line note))
       ;; Print out a new row with underlines connecting
       ;; back to the multiline position.
       (print-multiline-note-start printer-state note)
       ;; Push this note on to the stack for safe keeping.
       (push note note-stack)
       (incf note-current-depth))
      ;; For non-multiline just print the note
      (t
       ;; Print lines until this note.
       (print-lines-until printer-state (source-error-resolved-note-start-line note))
       (print-singleline-note printer-state note)
       (print-finished-multiline-notes-for-line printer-state (source-error-resolved-note-start-line note))))))

(defun print-help (printer-state help)
  (with-slots (stream source-stream) printer-state
    (with-slots (span message replacement) help
      (destructuring-bind (start . end) span
        (multiple-value-bind (start-line start-line-start)
            (get-line-from-index source-stream start)
          (multiple-value-bind (end-line end-line-start)
              (get-line-from-index source-stream (1- end))
            (unless (= start-line end-line)
              (implementation-bug "multiline help messages not supported yet."))
            (let ((line-number-width (1+ (floor (log end-line 10)))))
              (format stream "help: ~A~%" message)
              (format stream " ~vD | ~A"
                      line-number-width
                      start-line
                      (subseq (get-nth-line source-stream start-line)
                              0 (- start start-line-start)))
              (let ((replaced-text (funcall replacement
                                            (subseq (get-nth-line source-stream start-line)
                                                    (- start start-line-start)
                                                    (- end end-line-start)))))
                (format stream "~A~A~%"
                        replaced-text
                        (subseq (get-nth-line source-stream start-line)
                                (- end end-line-start)))
                (format stream " ~v{~C~:*~} |~v{~C~:*~}~v{~C~:*~}~%"
                        line-number-width '(#\Space)
                        (1+ (- start start-line-start)) '(#\Space)
                        (length replaced-text) '(#\-))))))))))

(defun print-source-error (stream error-info)
  (with-source-stream (source-stream error-info)
    (let ((state (make-instance 'printer-state
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

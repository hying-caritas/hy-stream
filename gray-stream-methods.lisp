;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-
;;;
;;; --- Implementation using Gray streams.
;;;

(in-package :hy-stream)

(defmacro check-bounds (sequence start end)
  (with-gensyms (length)
    `(let ((,length (length ,sequence)))
       (check-type ,start unsigned-byte "a non-negative integer")
       (when ,end (check-type ,end unsigned-byte "a non-negative integer or NIL"))
       (unless ,end
         (setf ,end ,length))
       (unless (<= ,start ,end ,length)
         (error "Wrong sequence bounds. start: ~S end: ~S" ,start ,end)))))

;;;-------------------------------------------------------------------------
;;; Instance Initialization
;;;-------------------------------------------------------------------------

(defun free-stream-buffers (ibuf obuf)
  (when ibuf (free-iobuf ibuf))
  (when obuf (free-iobuf obuf)))

;;; TODO: use the buffer pool
;;; TODO: handle instance reinitialization
(defmethod shared-initialize :after ((stream fd-stream) slot-names
                                     &key external-format input-buffer-size output-buffer-size)
  (declare (ignore slot-names))
  (let ((external-format (or external-format :default))
        (input-buffer-size (or input-buffer-size +bytes-per-iobuf+))
        (output-buffer-size (or output-buffer-size +bytes-per-iobuf+)))
   (check-type input-buffer-size buffer-index)
   (check-type output-buffer-size buffer-index)
   (with-slots ((ibuf-size input-buffer-size)
                (obuf-size output-buffer-size))
       stream
     (setf ibuf-size input-buffer-size
           obuf-size output-buffer-size))
   (setf (external-format-of stream) external-format)))


;;;-------------------------------------------------------------------------
;;; PRINT-OBJECT
;;;-------------------------------------------------------------------------

(defmethod print-object ((o fd-stream) s)
  (with-slots (fd (ef external-format) (ib input-buffer) (ob output-buffer)
                  (ib-size input-buffer-size) (ob-size output-buffer-size))
      o
    (print-unreadable-object (o s :type nil :identity t)
      (if fd
          (format s "~A ~S ~S ~S ~S/~S ~S ~S/~S ~S (~S ~S ~S)"
                  (type-of o) :fd fd
                  :ibuf (if ib (iobuf-length ib) 0) ib-size
                  :obuf (if ob (iobuf-length ob) 0) ob-size
                  :ef (babel-encodings:enc-name (babel:external-format-encoding ef))
                  :eol-style (babel:external-format-eol-style ef))
          (format s "~A ~A ~S (~S ~S ~S)"
                  (type-of o) :closed
                  :ef (babel-encodings:enc-name (babel:external-format-encoding ef))
                  :eol-style (babel:external-format-eol-style ef))))))


;;;-------------------------------------------------------------------------
;;; Common Methods
;;;-------------------------------------------------------------------------

(defmethod stream-element-type ((stream fd-stream))
  'character)

(defun fd-stream-ref (fd-stream)
  (with-fd-stream-locked fd-stream
    (with-slots ((refcount refcount))
        fd-stream
      (assert (/= 0 refcount))
      (incf refcount))))

(defun %fd-stream-unref (fd-stream)
  (with-fd-stream-locked fd-stream
    (with-slots ((refcount refcount))
        fd-stream
      (assert (/= 0 refcount))
      (decf refcount))))

;; TODO: use the buffer pool
(defmethod close :around ((stream fd-stream) &key abort)
  (unless (or abort (null (slot-value stream 'output-buffer)))
    (finish-output stream))
  (when (= (%fd-stream-unref stream) 0)
    (with-slots ((ibuf input-buffer)
                 (obuf output-buffer)
                 fd)
        stream
      (free-stream-buffers ibuf obuf)
      (setf ibuf nil obuf nil)
      (when fd
        (isys:close fd)))
    (call-next-method))
  t)

(defmethod open-stream-p ((stream fd-stream))
  (/= (slot-value stream 'refcount) 0))

(defmethod (setf external-format-of)
    (external-format (stream fd-stream))
  (with-fd-stream-locked stream
    (let ((canonical-ef (babel:ensure-external-format external-format)))
      (setf (slot-value stream 'external-format) canonical-ef)
      (setf (slot-value stream 'eol-writer)
            (case (babel:external-format-eol-style canonical-ef)
              (:lf   #'stream-write-lf)
              (:crlf #'stream-write-crlf)
              (:cr   #'stream-write-cr)))
      (setf (values (slot-value stream 'eol-finder)
                    (slot-value stream 'eol-finder/no-hang))
            (case (babel:external-format-eol-style canonical-ef)
              (:lf   (values #'stream-find-lf   #'stream-find-lf/no-hang))
              (:crlf (values #'stream-find-crlf #'stream-find-crlf/no-hang))
              (:cr   (values #'stream-find-cr   #'stream-find-cr/no-hang)))))))


;;;-------------------------------------------------------------------------
;;; Input Methods
;;;-------------------------------------------------------------------------

(defmethod stream-clear-input ((stream fd-stream))
  (with-fd-stream-input-locked stream
    (iobuf-reset (input-buffer-of stream))))

(declaim (inline %read-sequence))
(defun %read-sequence (stream seq start end)
  (check-bounds seq start end)
  (if (= start end)
      start
      (etypecase seq
        (ub8-sarray (%read-into-simple-array-ub8 stream seq start end))
        (string     (%read-into-string stream seq start end))
        (ub8-vector (%read-into-vector stream seq start end)))))

(defmethod stream-read-sequence
    ((stream fd-stream) sequence start end &key)
  (with-fd-stream-input-locked stream
    (%read-sequence stream sequence start end)))

(defmethod drain-input-buffer
    ((stream fd-stream) sequence &key (start 0) end)
  (check-bounds sequence start end)
  (with-fd-stream-input-locked stream
    (with-accessors ((ib input-buffer-of))
        stream
      (let ((nbytes (min (- end start)
                         (iobuf-length ib))))
        (when (plusp nbytes)
          (iobuf-copy-into-lisp-array ib (iobuf-start ib)
                                      sequence start
                                      nbytes)
          (incf (iobuf-start ib) nbytes)
          (let ((len (iobuf-length ib)))
            (values (+ start nbytes)
                    (and (plusp len) len))))))))


;;;-------------------------------------------------------------------------
;;; Output Methods
;;;-------------------------------------------------------------------------

(defmethod stream-clear-output ((stream fd-stream))
  (with-fd-stream-output-locked stream
    (iobuf-reset (output-buffer-of stream))
    (setf (dirtyp stream) nil)))

(defmethod stream-finish-output ((stream fd-stream))
  (with-fd-stream-output-locked stream
    (with-accessors ((fd fd-of)
                     (ob output-buffer-of)
                     (dirtyp dirtyp))
        stream
      (with-hangup-guard stream
        (%write-octets-from-iobuf fd ob))
      (setf dirtyp nil))))

(defmethod stream-force-output ((stream fd-stream))
  (with-fd-stream-output-locked stream
    (with-accessors ((fd fd-of)
                     (ob output-buffer-of)
                     (dirtyp dirtyp))
        stream
      (with-hangup-guard stream
        (%write-octets-from-iobuf fd ob))
      (unless (iobuf-empty-p ob)
        (setf dirtyp t)))))

(declaim (inline %write-sequence))
(defun %write-sequence (stream seq start end)
  (check-bounds seq start end)
  (if (= start end)
      seq
      (etypecase seq
        (ub8-sarray (%write-simple-array-ub8 stream seq start end))
        (string     (stream-write-string stream seq start end))
        (ub8-vector (%write-vector-ub8 stream seq start end))
        (vector     (%write-vector stream seq start end)))))

(defmethod stream-write-sequence ((stream fd-stream)
                                  sequence start end &key)
  (with-fd-stream-output-locked stream
    (%write-sequence stream sequence start end)))


;;;-------------------------------------------------------------------------
;;; Character Input
;;;-------------------------------------------------------------------------

(defun %stream-rewind-iobuf (stream iobuf encoding)
  (maybe-rewind-iobuf iobuf encoding)
  (setf (unread-index-of stream) (iobuf-start iobuf)))

(defun %stream-read-char (stream)
  (multiple-value-bind (ef eol-finder)
      (with-fd-stream-locked stream
        (values (external-format-of stream)
                (eol-finder-of stream)))
    (with-accessors ((fd fd-of)
                     (ib input-buffer-of)
                     (unread-index unread-index-of))
        stream
      (let ((encoding (babel:external-format-encoding ef)))
        (%stream-rewind-iobuf stream ib encoding)
        (cond
          ((and (iobuf-empty-p ib)
                (eql :eof (%fill-ibuf ib fd)))
           :eof)
          (t
           ;; At this point, there's at least one octet in the buffer
           (assert (not (iobuf-empty-p ib)))
           (let ((line-end (funcall eol-finder ib fd)))
             (if (eql #\Newline line-end)
                 #\Newline
                 (decode-one-char fd ib encoding)))))))))

(defmethod stream-read-char ((stream fd-stream))
  (with-fd-stream-input-locked stream
    (%stream-read-char stream)))

(defun %stream-read-char-no-hang (stream)
  (multiple-value-bind (ef eol-finder/no-hang)
      (with-fd-stream-locked stream
        (values (external-format-of stream)
                (eol-finder/no-hang-of stream)))
    (with-accessors ((fd fd-of)
                     (ib input-buffer-of))
        stream
      (let ((encoding (babel:external-format-encoding ef)))
        (%stream-rewind-iobuf stream ib encoding)
        (when (iobuf-empty-p ib)
          (let ((nbytes (%fill-ibuf/no-hang ib fd)))
            (cond
              ((eql :eof nbytes) (return-from %stream-read-char-no-hang :eof))
              ((zerop nbytes)    (return-from %stream-read-char-no-hang nil)))))
        ;; At this point, there's at least one octet in the buffer
        (assert (not (iobuf-empty-p ib)))
        (let ((line-end (funcall eol-finder/no-hang ib fd)))
          (case line-end
            ((nil) (decode-one-char/no-hang ib encoding))
            (#\Newline #\Newline)
            ;; There's a CR but it's not EOF so we could still receive a LF
            (:incomplete nil)))))))

(defmethod stream-read-char-no-hang ((stream fd-stream))
  (with-fd-stream-input-locked stream
    (%stream-read-char-no-hang stream)))

(defun %stream-unread-char (stream)
  (declare (type fd-stream stream))
  (with-accessors ((ib input-buffer-of)
                   (unread-index unread-index-of))
      stream
    (symbol-macrolet ((start (iobuf-start ib)))
      (cond
        ((> start unread-index)
         (setf start unread-index))
        ((= start unread-index)
         (error 'no-characters-to-unread :stream stream))
        (t (error "On stream ~S the buffer start(~A) is less than the unread index(~A)."
                  stream start unread-index)))))
  nil)

(defmethod stream-unread-char ((stream fd-stream) character)
  (declare (ignore character))
  (with-fd-stream-input-locked stream
    (%stream-unread-char stream)))

(defmethod stream-peek-char ((stream fd-stream))
  (with-fd-stream-input-locked stream
    (let ((char (%stream-read-char stream)))
      (cond ((eql :eof char)
             :eof)
            (t
             (%stream-unread-char stream)
             char)))))

;; (defmethod stream-read-line ((stream fd-stream))
;;   )

(defmethod stream-listen ((stream fd-stream))
  (with-fd-stream-input-locked stream
    (let ((char (stream-read-char-no-hang stream)))
      (cond ((characterp char) (stream-unread-char stream char) t)
            ((eql :eof char) nil)
            (t t)))))


;;;-------------------------------------------------------------------------
;;; Character Output
;;;-------------------------------------------------------------------------

(defmethod stream-write-char ((stream fd-stream)
                              (character character))
  (multiple-value-bind (ef eol-writer)
      (with-fd-stream-locked stream
        (values (external-format-of stream)
                (eol-writer-of stream)))
    (with-fd-stream-output-locked stream
      (%flush-obuf-if-needed stream)
      (if (char= character #\Newline)
          (funcall eol-writer stream)
          (let ((string (make-string 1 :initial-element character)))
            (declare (dynamic-extent string))
            (%stream-write-string stream string 0 nil ef eol-writer))))))

(defmethod stream-line-column ((stream fd-stream))
  0)

(defmethod stream-start-line-p ((stream fd-stream))
  nil)

(defmethod stream-terpri ((stream fd-stream))
  (write-char #\Newline stream) nil)

(defmethod stream-fresh-line ((stream fd-stream))
  (write-char #\Newline stream) t)

(defun %stream-write-string (stream string start end ef eol-writer)
  (check-bounds string start end)
  (do* ((encoding (babel:external-format-encoding ef)))
       ((= start end))
    (case (char string start)
      (#\Newline
       (funcall eol-writer stream)
       (incf start))
      (t
       (setf start (%write-string-chunk stream string start end encoding)))))
  string)

(defmethod stream-write-string ((stream fd-stream)
                                (string string) &optional (start 0) end)
  (multiple-value-bind (ef eol-writer)
      (with-fd-stream-locked stream
        (values (external-format-of stream)
                (eol-writer-of stream)))
    (with-fd-stream-output-locked stream
      (%stream-write-string stream string start end ef eol-writer))))


;;;-------------------------------------------------------------------------
;;; Binary Input
;;;-------------------------------------------------------------------------

(defun %stream-read-byte (stream)
  (with-accessors ((fd fd-of)
                   (ib input-buffer-of))
      stream
    (flet ((fill-buf-or-eof ()
             (iobuf-reset ib)
             (when (eql :eof (%fill-ibuf ib fd))
               (return-from %stream-read-byte :eof))))
      (when (zerop (iobuf-length ib))
        (fill-buf-or-eof))
      (iobuf-pop-octet ib))))

(defmethod stream-read-byte ((stream fd-stream))
  (with-fd-stream-input-locked stream
    (%stream-read-byte stream)))


;;;-------------------------------------------------------------------------
;;; Binary Output
;;;-------------------------------------------------------------------------

(defun %stream-write-byte (stream integer)
  (check-type integer ub8 "an unsigned 8-bit value")
  (with-accessors ((ob output-buffer-of))
      stream
    (with-hangup-guard stream
      (%flush-obuf-if-needed stream))
    (iobuf-push-octet ob integer)))

(defmethod stream-write-byte ((stream fd-stream) integer)
  (with-fd-stream-output-locked stream
    (%stream-write-byte stream integer)))


;;;-------------------------------------------------------------------------
;;; Buffer-related functions
;;;-------------------------------------------------------------------------

(defmethod input-buffer-size ((stream fd-stream))
  (with-fd-stream-input-locked stream
    (iobuf-length (input-buffer-of stream))))

(defmethod input-buffer-empty-p ((stream fd-stream))
  (with-fd-stream-input-locked stream
   (iobuf-empty-p (input-buffer-of stream))))

(defmethod output-buffer-size ((stream fd-stream))
  (with-fd-stream-output-locked stream
   (iobuf-length (output-buffer-of stream))))

(defmethod output-buffer-empty-p ((stream fd-stream))
  (with-fd-stream-output-locked stream
   (iobuf-empty-p (output-buffer-of stream))))

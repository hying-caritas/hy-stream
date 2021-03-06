;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-
;;;
;;; --- Low-level I/O functions.
;;;

(in-package :hy-stream)

;;;-------------------------------------------------------------------------
;;; Input
;;;-------------------------------------------------------------------------

(declaim (inline %read-once))
(defun %read-once (fd iobuf)
  (declare (type iobuf iobuf))
  (isys:read fd (iobuf-end-pointer iobuf)
             (iobuf-end-space-length iobuf)))

(declaim (inline %fill-ibuf))
(defun %fill-ibuf (iobuf fd)
  (declare (type iobuf iobuf))
  (let ((nbytes (%read-once fd iobuf)))
    (if (zerop nbytes)
        :eof
        (progn (incf (iobuf-end iobuf) nbytes) nbytes))))

(declaim (inline %read-once/no-hang))
(defun %read-once/no-hang (fd iobuf)
 (declare (type iobuf iobuf))
 (when (iomux:fd-readablep fd)
   (isys:read fd (iobuf-end-pointer iobuf)
              (iobuf-end-space-length iobuf))))

(declaim (inline %fill-ibuf/no-hang))
(defun %fill-ibuf/no-hang (iobuf fd)
  (declare (type iobuf iobuf))
  (let ((nbytes (%read-once/no-hang fd iobuf)))
    (cond
      ((null nbytes)
       0)
      ((plusp nbytes)
       (incf (iobuf-end iobuf) nbytes)
       nbytes)
      ((zerop nbytes)
       :eof))))

(defun %read-into-simple-array-ub8 (stream array start end)
  (declare (type fd-stream stream))
  (with-accessors ((fd fd-of)
                   (iobuf input-buffer-of))
      stream
    (let ((octets-needed (- end start)))
      (loop :with array-offset := start
         :for octets-in-buffer := (iobuf-length iobuf)
         :for nbytes := (min octets-needed octets-in-buffer)
         :when (plusp nbytes) :do
         (iobuf-copy-into-lisp-array iobuf (iobuf-start iobuf)
                                     array array-offset nbytes)
         (incf array-offset nbytes)
         (decf octets-needed nbytes)
         (incf (iobuf-start iobuf) nbytes)
         :if (zerop octets-needed) :do (loop-finish)
         :else :do (iobuf-reset iobuf)
         :when (eql :eof (%fill-ibuf iobuf fd))
         :do (loop-finish)
         :finally (return array-offset)))))

(defun %read-into-string (stream string start end)
  (declare (type fd-stream stream))
  (loop :for offset :from start :below end
        :for char := (%stream-read-char stream)
     :if (eql :eof char) :do (loop-finish)
     :else :do (setf (char string offset) char)
     :finally (return offset)))

(defun %read-into-vector (stream vector start end)
  (declare (type fd-stream stream))
  (loop :for offset :from start :below end
        :for octet := (%stream-read-byte stream)
     :if (eql :eof octet) :do (loop-finish)
     :else :do (setf (aref vector offset) octet)
     :finally (return offset)))


;;;-------------------------------------------------------------------------
;;; Output
;;;-------------------------------------------------------------------------

(defmacro with-hangup-guard (stream &body body)
  (with-gensyms (bytes-written hangup-p)
    `(multiple-value-bind (,bytes-written ,hangup-p)
         (progn ,@body)
       (declare (ignore ,bytes-written))
       (when (eql :hangup ,hangup-p)
         (error 'hangup :stream ,stream)))))

(defun %write-octets-from-foreign-memory (fd buf start end)
  (declare (type stream-buffer buf))
  (let ((old-start start))
    (do () ((= start end) (- start old-start))
      (handler-case
          (incf start (isys:write fd (inc-pointer buf start) (- end start)))
        (isys:epipe ()
          (return (values (- start old-start) :hangup)))))))

(defun %write-octets-from-iobuf (fd iobuf)
  (declare (type iobuf iobuf))
  (multiple-value-bind (bytes-written hangup-p)
      (%write-octets-from-foreign-memory
       fd (iobuf-data iobuf) (iobuf-start iobuf) (iobuf-end iobuf))
    (incf (iobuf-start iobuf) bytes-written)
    (when (iobuf-empty-p iobuf) (iobuf-reset iobuf))
    (values bytes-written hangup-p)))

(defun %flush-obuf-if-needed (stream)
  (declare (type fd-stream stream))
  (with-accessors ((fd fd-of)
                   (iobuf output-buffer-of)
                   (dirtyp dirtyp))
      stream
    (when (or dirtyp (iobuf-full-p iobuf))
      (multiple-value-bind (bytes-written hangup-p)
          (%write-octets-from-iobuf fd iobuf)
        (setf dirtyp nil)
        (return-from %flush-obuf-if-needed (values bytes-written hangup-p))))
    0))

(defun %write-simple-array-ub8 (stream array start end)
  (declare (type fd-stream stream))
  (with-accessors ((fd fd-of)
                   (iobuf output-buffer-of))
      stream
    (cond ((iobuf-can-fit-slice-p iobuf start end)
           (iobuf-append-slice iobuf array start end))
          (t
           (with-hangup-guard stream
             (%write-octets-from-iobuf fd iobuf))
           (with-pointer-to-vector-data (ptr array)
             (with-hangup-guard stream
               (%write-octets-from-foreign-memory fd ptr start end)))))
    array))

(defun %write-vector-ub8 (stream vector start end)
  (declare (type fd-stream stream))
  (%write-simple-array-ub8 stream (coerce vector 'ub8-sarray) start end))

(defun %write-vector (stream vector start end)
  (declare (type fd-stream stream))
  (loop :for offset :from start :below end
        :for octet := (aref vector offset)
     :do (%stream-write-byte stream octet)
     :finally (return vector)))


;;;-------------------------------------------------------------------------
;;; Character Input
;;;-------------------------------------------------------------------------

(defun stream-find-lf (iobuf fd)
  (declare (ignore fd)
           (type iobuf iobuf))
  (assert (plusp (iobuf-length iobuf)))
  (cond
    ((= (iobuf-peek iobuf) #.(char-code #\Linefeed))
     (incf (iobuf-start iobuf))
     #\Newline)
    (t nil)))

(setf (fdefinition 'stream-find-lf/no-hang) #'stream-find-lf)

(defun stream-find-cr (iobuf fd)
  (declare (ignore fd)
           (type iobuf iobuf))
  (assert (plusp (iobuf-length iobuf)))
  (cond
    ((= (iobuf-peek iobuf) #.(char-code #\Return))
     (incf (iobuf-start iobuf))
     #\Newline)
    (t nil)))

(setf (fdefinition 'stream-find-cr/no-hang) #'stream-find-cr)

(defun stream-find-crlf (iobuf fd)
  (declare (type iobuf iobuf))
  (assert (plusp (iobuf-length iobuf)))
  (cond
    ((/= (iobuf-peek iobuf) #.(char-code #\Return))
     nil)
    ((and (= 1 (iobuf-length iobuf))
          (eql :eof (%fill-ibuf iobuf fd)))
     nil)
    ((= (iobuf-peek iobuf 1) #.(char-code #\Linefeed))
     (incf (iobuf-start iobuf) 2)
     #\Newline)
    (t nil)))

(defun stream-find-crlf/no-hang (iobuf fd)
  (declare (type iobuf iobuf))
  (assert (plusp (iobuf-length iobuf)))
  (cond
    ((/= (iobuf-peek iobuf) #.(char-code #\Return))
     nil)
    ((= 1 (iobuf-length iobuf))
     (if (eql :eof (%fill-ibuf/no-hang iobuf fd))
         nil
         :incomplete))
    ((= (iobuf-peek iobuf 1) #.(char-code #\Linefeed))
     (incf (iobuf-start iobuf) 2)
     #\Newline)
    (t nil)))

(defun maybe-rewind-iobuf (iobuf encoding)
  (let ((max-octets-per-char
         (babel-encodings:enc-max-units-per-char encoding)))
    ;; Some encodings such as CESU or Java's modified UTF-8 take
    ;; as much as 6 bytes per character. Make sure we have enough
    ;; space to collect read-ahead bytes if required.
    (when (< (- (iobuf-size iobuf)
                (iobuf-start iobuf))
             max-octets-per-char)
      (iobuf-copy-data-to-start iobuf))))

(defun decode-one-char (fd iobuf encoding)
  (assert (plusp (iobuf-length iobuf)))
  (loop
     (handler-case
         (multiple-value-bind (str ret)
             (foreign-string-to-lisp (iobuf-data iobuf)
                                     :offset (iobuf-start iobuf)
                                     :count (iobuf-length iobuf)
                                     :encoding encoding
                                     :max-chars 1)
           (incf (iobuf-start iobuf) ret)
           (return-from decode-one-char (char str 0)))
       (babel:end-of-input-in-character ()
         (let ((nbytes (%fill-ibuf iobuf fd)))
           ;; Even if the buffer contains octets representing an
           ;; incomplete character, we return EOF because the Gray
           ;; streams API doesn't distinguish between "no data" and
           ;; "partial data"
           (when (eql :eof nbytes)
             (return-from decode-one-char :eof)))))))

(defun decode-one-char/no-hang (iobuf encoding)
  (assert (plusp (iobuf-length iobuf)))
  (handler-case
      (multiple-value-bind (string ret)
          (foreign-string-to-lisp (iobuf-data iobuf)
                                  :offset (iobuf-start iobuf)
                                  :count (iobuf-length iobuf)
                                  :encoding encoding
                                  :max-chars 1)
        (incf (iobuf-start iobuf) ret)
        (char string 0))
    (babel:end-of-input-in-character () nil)))


;;;-------------------------------------------------------------------------
;;; Character Output
;;;-------------------------------------------------------------------------

(defun stream-write-lf (stream)
  (declare (type fd-stream stream))
  (let ((octets #.(map 'ub8-sarray #'char-code '(#\Linefeed))))
    (%write-simple-array-ub8 stream octets 0 1)))

(defun stream-write-crlf (stream)
  (declare (type fd-stream stream))
  (let ((octets #.(map 'ub8-sarray #'char-code '(#\Return #\Linefeed))))
    (%write-simple-array-ub8 stream octets 0 2)))

(defun stream-write-cr (stream)
  (declare (type fd-stream stream))
  (let ((octets #.(map 'ub8-sarray #'char-code '(#\Return))))
    (%write-simple-array-ub8 stream octets 0 1)))

(declaim (inline %write-string-chunk))
(defun %write-string-chunk (stream string start end encoding)
  (let* ((chunk-end
          (or (position #\Newline string :start start :end end) end))
         (octets
          (babel:string-to-octets string
                                  :start start :end chunk-end
                                  :encoding encoding)))
    (%write-simple-array-ub8 stream octets 0 (length octets))
    chunk-end))

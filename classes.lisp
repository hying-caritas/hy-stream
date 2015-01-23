;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-
;;;
;;; --- fd-streams classes.
;;;

(in-package :hy-stream)

;;;; Stream Buffers

(deftype stream-buffer () 'foreign-pointer)
(deftype buffer-index () '(unsigned-byte 24))

(defstruct (iobuf (:constructor %make-iobuf ()))
  (data  (null-pointer) :type stream-buffer)
  (size  0              :type buffer-index)
  (start 0              :type buffer-index)
  (end   0              :type buffer-index))

;;;; Bivalent Socket Gray Stream

(defclass fd-stream (fundamental-binary-input-stream
                     fundamental-binary-output-stream
                     fundamental-character-input-stream
                     fundamental-character-output-stream)
  ((fd :initform nil :initarg :fd :accessor fd-of)
   ;; protect refcount, ef, eol-xxx
   (lock :initform (make-lock))
   (refcount :initform 1 :type integer)
   (external-format :initform :default :initarg :external-format
                    :reader external-format-of
                    :documentation "placehold")
   (eol-writer :reader eol-writer-of)
   (eol-finder :reader eol-finder-of)
   (eol-finder/no-hang :reader eol-finder/no-hang-of)
   ;; protect input-buffer and unread-index
   (input-lock :initform (make-lock))
   (input-buffer :initform nil :type (or iobuf null))
   (input-buffer-size :type integer)
   ;; Last read char buffer index.
   (unread-index :initform 0 :type buffer-index
                 :accessor unread-index-of)
   ;; protect output-buffer and dirty
   (output-lock :initform (make-lock))
   (output-buffer :initform nil :type (or iobuf null))
   (output-buffer-size :type integer)
   ;; Flag used by stream-force-output.
   (dirty :initform nil :type boolean :accessor dirtyp))
  (:documentation "File descriptor stream"))

(deftype stream-position () '(unsigned-byte 64))

(deftype ub8  () '(unsigned-byte 8))
(deftype ub8-sarray (&optional (size '*))
  `(simple-array ub8 (,size)))
(deftype ub8-vector (&optional (size '*))
  `(vector ub8 ,size))

(defmacro with-fd-stream-locked (fd-stream &body body)
  `(with-lock-held ((slot-value ,fd-stream 'lock))
     ,@body))

(defmacro with-fd-stream-input-locked (fd-stream &body body)
  `(with-lock-held ((slot-value ,fd-stream 'input-lock))
     ,@body))

(defmacro with-fd-stream-output-locked (fd-stream &body body)
  `(with-lock-held ((slot-value ,fd-stream 'output-lock))
     ,@body))

(defgeneric (setf external-format-of) (external-format stream)
  (:documentation "placeholder"))

(defgeneric drain-input-buffer (stream sequence &key start end)
  (:documentation ""))

(defgeneric input-buffer-size (stream)
  (:documentation ""))

(defgeneric input-buffer-empty-p (stream)
  (:documentation ""))

(defgeneric output-buffer-size (stream)
  (:documentation ""))

(defgeneric output-buffer-empty-p (stream)
  (:documentation ""))

(defun input-buffer-of (stream)
  (with-slots (input-buffer input-buffer-size)
      stream
    (unless input-buffer
      (setf input-buffer (allocate-iobuf input-buffer-size)))
    input-buffer))

(defun output-buffer-of (stream)
  (with-slots (output-buffer output-buffer-size)
      stream
    (unless output-buffer
      (setf output-buffer (allocate-iobuf output-buffer-size)))
    output-buffer))

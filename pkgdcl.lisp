;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-
;;;
;;; --- Package definition.
;;;

(in-package :common-lisp-user)

(defpackage :hy-stream
  (:use :common-lisp :cffi :bordeaux-threads :trivial-gray-streams)
  (:import-from #:alexandria #:with-gensyms)
  (:export
   ;; Classes
   #:fd-stream

   ;; Conditions
   #:hangup
   #:no-characters-to-unread

   ;; Accessors
   #:external-format-of
   #:fd-of
   #:read-buffer-size
   #:read-buffer-empty-p
   #:write-buffer-size
   #:write-buffer-empty-p

   #:fd-stream-ref
   #:drain-input-buffer))

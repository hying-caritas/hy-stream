;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-

(asdf:defsystem :hy-stream
  :description "Huang Ying's File Descriptor Gray Stream"
  :maintainer "Huang Ying <ying.huang@intel.com>"
  :version "0.1"
  :licence "MIT"
  :depends-on (:iolib/syscalls :iolib/multiplex :cffi :bordeaux-threads
               :trivial-gray-streams :alexandria)
  :components
  ((:file "pkgdcl")
   (:file "classes" :depends-on ("pkgdcl"))
   (:file "conditions" :depends-on ("pkgdcl"))
   (:file "buffer" :depends-on ("pkgdcl" "classes"))
   (:file "io-helpers"
     :depends-on ("pkgdcl" "classes" "conditions" "buffer"))
   (:file "gray-stream-methods"
     :depends-on ("pkgdcl" "classes" "conditions" "buffer"
                  "io-helpers"))))

(in-package #:cl-user)

(defpackage #:payload
  (:documentation "file hash and compress and send utility")
  (:use #:cl)
  (:export
   ;; parameters
   *project-name*
   *operations-home*
   *remote-login*
   *target-dir*
   *payload-files*
   *log*
   ;; functions
   check-parameters
   create-transfer-directories
   create-documents
   test-transfer
   check-payload
   link-all
   hash-all
   compress
   transfer
   cleanup
   deliver-payload
   ))

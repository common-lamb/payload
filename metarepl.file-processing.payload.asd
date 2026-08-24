(defsystem "metarepl.file-processing.payload"
  :description "file hash compres and send utility"
  :author "metarepl (https://github.com/metarepl)"
  :version "0.0.1"
  :license "MIT"
  :depends-on (
               :cmd
               :str
               :cl-ppcre
               :chipz
               :alexandria
               :journal
               :local-time
               :filepaths
               :filesystem-utils
               )
  :serial t
  :components ((:file "package")
               (:file "payload")))

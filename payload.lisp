(in-package :payload)

(defparameter *remote-login* ""
  "
like: user@remote: with trailing colon
or empty string if local
")

(defparameter *target* nil
  "
an absolute file path as string with trailing slash")

(defparameter *payload* nil
  "list of file paths (no dirs)")

;;;; check dependencies
(defun check-dependencies ()
  (cmd:$cmd "which rsync")
  (cmd:$cmd "which md5sum")
  ;; all good
  T)

;; USER (check-dependencies)

;;;; create dir structure
(defparameter *date*
  (local-time:format-timestring nil (local-time:now) :format local-time:+iso-8601-date-format+))
(defun transfer-dir-name ()
  (format nil "~A_data-transfer/" *date*))

(defun transfer-dir ()
  (filepaths:join (user-homedir-pathname) (transfer-dir-name)))
(defun payload-dir ()
  (filepaths:join (transfer-dir) "payload/" ))
(defun docs-dir ()
  (filepaths:join (transfer-dir) "docs/" ))
(defun hashes-dir ()
  (filepaths:join (transfer-dir) "hashes/" ))

(defun create-transfer-directories ()
  (ensure-directories-exist (transfer-dir))
  (ensure-directories-exist (payload-dir))
  (ensure-directories-exist (docs-dir))
  (ensure-directories-exist (hashes-dir)))

;; USER (create-transfer-directories)

;;;; define commands
(defparameter *cmd-compress* "tar -czvf")
(defparameter *cmd-decompress* "tar -xzvf")
(defparameter *cmd-hash* "md5sum * > ../hashes/checksum.md5")
(defparameter *cmd-hash-check* "md5sum -c")

(defun call-rsync (file target &key (flags "-vrPL --dry-run"))
  "
ARGS:
file: string like /path/to/file.type
target: string like user@remote:/path/to/dest/
flags: string like -vrPLn
"
  (uiop:run-program (format nil "rsync ~A ~A ~A" flags file target)
                       :output :interactive
                       :input :interactive))

;;;; documents
(defun make-documents ()
  "
creates the documents for the recievers
"
  ;; make decompress doc file
  (with-open-file (out (filepaths:join (docs-dir) "compression-docs.txt" )
                       :direction :output
                       :if-exists :supersede
                       :if-does-not-exist :create)
    (write-line (format nil "Greetings!~%") out)
    (write-line (format nil "compressed with:~%  ~A <directory>" *cmd-compress*) out)
    (write-line (format nil "decompress with:~%  ~A <compressed>.tar.gz" *cmd-decompress*) out))

  ;; make hash doc file
  (with-open-file (out (filepaths:join (docs-dir) "hash-docs.txt" )
                       :direction :output
                       :if-exists :supersede
                       :if-does-not-exist :create)
    (write-line (format nil "With great regards!~%") out)
    (write-line (format nil "hashed with:~%  ~A" *cmd-hash*) out)
    (write-line (format nil "nav to payload dir and check hashes with:~%  ~A ../hashes/checksum.md5 " *cmd-hash-check*) out)
    T))

;; USER (make-documents)

;;;; check target has write access
(defun test-transfer ()
  "
rsync a file to target to test
errors on code 23 if no write access
"
  (let ((target (format nil "~A~A" *remote-login* *target*))
        (file (namestring (filepaths:join (docs-dir) "compression-docs.txt"))))

    (format t "Testing target dir write access:~&  ~A" target)
    (call-rsync file target :flags "-vrPL")))

;; USER (test-transfer)

;;;; check each in payload exists &&&
;; (mapcar #'probe-file *payload*)

;;;; symlink each in payload into payload dir &&&
(defun make-symlink (file)
  "
creates a symlink in payload
"
  (cmd:cmd (format nil
                   "ln -s '~A' '~A'"
                   (namestring
                    (uiop:ensure-absolute-pathname file))
                   (namestring
                    (payload-dir)))))

;; (mapcar #'make-symlink *payload*)

;;;; hash each in payload into hashes dir

(defun hash-file (file)
  (format t "~&Hashing:~%  '~A'" file)
  (cmd:cmd (format nil
                   "md5sum '~A' >> ../hashes/checksum.md5"
                   (namestring file))))

(defun hash-payload ()
  (uiop:with-current-directory  ((payload-dir))

    (cmd:cmd "echo > ../hashes/checksum.md5") ;; clean start
    (mapcar #'hash-file
              (uiop:directory-files (uiop:getcwd)))))

;; USER (hash-payload)

;;;; recursive compress data-transfer dir &&&


;;;; move dir to target &&&
;;;; clean up &&&

;;;; workflow

(in-package :payload)

;;;; logging
(defvar *log* nil)
(defparameter *log-level* 1)
(defparameter *debug* t)

(defun init-mem-log (&key (log-level 1))
  (let (
        (journal (journal:make-in-memory-journal))
        )
    (setf *log* journal)
    (setf *log-level* log-level)
    (journal:logged (*log*) "~%reinitialized memory log to log-level ~A" log-level)
    ))

(defun logged-fun (a)
  ;; (jrn:framed (check-dependencies :log-record (when (>= *log-level* 1) *log*)))
  (journal:framed(fun :log-record (when (>= *log-level* 1) *log*)
                      :args `(,a))
    (journal:logged ((when *debug* *log*)) "in fun ~A" a)
    (let ((return-value a))
      return-value)))

;;;; check dependencies
(defun check-dependencies ()
  (jrn:framed (check-dependencies :log-record (when (>= *log-level* 1) *log*))
    (cmd:$cmd "which tar")
    (cmd:$cmd "which rsync")
    (cmd:$cmd "which md5sum")
    (cmd:$cmd "which ln")
    ;; all good
    T))

(defvar *project-name* nil
  "name to prepend &&&")

(defvar *remote-login* ""
  "string like: user@remote:
with trailing colon or empty string if local")

(defvar *target-dir* nil
  "an absolute file path to a directory as string with trailing slash")

(defvar *payload-files* nil
  " a list of file paths (no dirs)")

(defvar *payload-dirs* nil
  "&&& not implemented list of file paths to dirs (no files))")

(defvar *operations-home* (user-homedir-pathname)
  "pathname used as toplevel of staging")

(defun check-parameters ()
  (jrn:framed (check-parameters :log-record (when (>= *log-level* 1) *log*))
    (journal:logged ((when *debug* *log*)) "project-name: ~A" *project-name*)
    (journal:logged ((when *debug* *log*)) "operations-home: ~A" *operations-home*)
    (journal:logged ((when *debug* *log*)) "remote-login: ~A" *remote-login*)
    (journal:logged ((when *debug* *log*)) "target-dir: ~A" *target-dir*)
    (let ((params '(*project-name*
                    *operations-home*
                    *remote-login*
                    *target-dir*)))
      ;; print current state
      (mapcar (lambda (p) (format t "~&~A: ~A~%" (symbol-name p) (symbol-value p)))
              params)
      ;; none nil
      (assert (every (lambda (p) (not (null (symbol-value p))))
                     params)
              (*project-name*
               *operations-home*
               *remote-login*
               *target-dir*) "parameters must be set and cannot be nil.")

      ;; "" or ends with :
      (assert (or (str:ends-with? ":" *remote-login*)
                  (string-equal "" *remote-login*))
              (*remote-login*)
              "*remote-login* can be an empty string for local transfers, if set it must end with :")
      ;; sends with /
      (assert (and (or (str:starts-with? "/" *target-dir*)
                       (str:starts-with? "~/" *target-dir*))
                   (str:ends-with? "/" *target-dir*))
              (*target-dir*)
              "*target-dir* must start with either ~ ~/ and must end with /")
      ;; must have some payload
      (assert (not (null *payload-files*))
              (*payload-files*)
              "*payload-files* must be a list with a filepath in it")
      )))

(defun check-payload ()
  "check payload contents are set and exist"
  (jrn:framed (check-payload :log-record (when (>= *log-level* 1) *log*))
    (journal:logged ((when *debug* *log*)) "payload-files: ~A" *payload-files*)
    ;; print current state
    (mapcar (lambda (f) (format t "~&file: ~A~%existsp: ~A" f (probe-file f)))
            *payload-files*)
    ;; at least one file
  (assert (not (null *payload-files*))
          (*payload-files*)
          "Please add at least one filepath to *payload-files*")
    ;; all payload files actually exist
    (assert (every (lambda (f) (not (null (probe-file f))))
                   *payload-files*)
            (*payload-files*) "all payload files must exist")
    ))

;;;; create dir names
(defun parent-namestring (path)
  "
takes a path
returns the single lowest directory namestring "
  (first (last (pathname-directory path))))

(defun make-transfer-dir-name ()
  (format nil "~A_data-transfer/" *project-name*))

(defun make-transfer-dir ()
  "dir for all collection and operation before sending"
  (filepaths:join *operations-home* (make-transfer-dir-name)))

(defun make-payload-dir ()
  "dir for collecting *payload-files* into"
  (filepaths:join (make-transfer-dir) "payload/" ))

(defun make-docs-dir ()
  "dir for docs"
  (filepaths:join (make-transfer-dir) "docs/" ))

(defun make-hashes-dir ()
  "dir for hashes"
  (filepaths:join (make-transfer-dir) "hashes/" ))

(defun make-compressed-name ()
  (str:concat (parent-namestring (make-transfer-dir)) ".tar.gz"))

;;; create dir structure
(defun create-transfer-directories ()
  (jrn:framed (create-transfer-directories :log-record (when (>= *log-level* 1) *log*))

    ;; (journal:logged ((when *debug* *log*)) "a: ~A" a)
    (check-dependencies)
    (check-parameters)
    (ensure-directories-exist (make-transfer-dir))
    (ensure-directories-exist (make-payload-dir))
    (ensure-directories-exist (make-docs-dir))
    (ensure-directories-exist (make-hashes-dir))
    (ensure-directories-exist (make-transfer-dir))
    ))

;;;; define commands
(defparameter *cmd-compress* "tar -chzvf") ;; -h to derefrence symlinks!
(defparameter *cmd-decompress* "tar -xzvf")
(defparameter *cmd-hash* "md5sum * > ../hashes/checksum.md5")
(defparameter *cmd-hash-check* "md5sum --check")

                                        ;
(defun call-rsync (file target &key (flags "-vrPL --dry-run"))
  "
ARGS:
file: string like /path/to/file.type
target: string like user@remote:/path/to/dest/
flags: string like -vrPL --dry-run
"
  (jrn:framed (call-rsync :log-record (when (>= *log-level* 1) *log*)
                          :args `(,file ,target))
    ;; (journal:logged ((when *debug* *log*)) "a: ~A" a)
    (handler-case
        (uiop:run-program (format nil "rsync ~A ~A ~A" flags file target)
                          :output :interactive
                          :input :interactive)
      (error (e)
        (format t "~&rsync Error: ~A~%" e)
        (format t "~&Error codes:~%")
        (format t "~&    code 23 if no write access~%")
        (format t "~&    code 3 if dir doesn't exist~%")
        (format t "~&    code 255 if *remote-connection* is wrongo bongo~%")
        ))))

;;;; documents
(defun create-documents ()
  " creates simple documents for the end users"
  (jrn:framed (create-dependencies :log-record (when (>= *log-level* 1) *log*))
    (journal:logged ((when *debug* *log*)) "make-docs-dir: ~A" (make-docs-dir))
    ;; make decompress doc file
    (with-open-file (out (filepaths:join (make-docs-dir) "compression-docs.txt" )
                         :direction :output
                         :if-exists :supersede
                         :if-does-not-exist :create)
      (write-line (format nil "Greetings!~%") out)
      (write-line (format nil "compressed with:~%  ~A <directory>" *cmd-compress*) out)
      (write-line (format nil "decompress with:~%  ~A <compressed-directory>.tar.gz" *cmd-decompress*) out))
  ;; make hash doc file
  (with-open-file (out (filepaths:join (make-docs-dir) "hash-docs.txt" )
                       :direction :output
                       :if-exists :supersede
                       :if-does-not-exist :create)
    (write-line (format nil "With our greatest regards!~%") out)
    (write-line (format nil "hashed with:~%  ~A" *cmd-hash*) out)
    (write-line (format nil "nav to payload dir and check hashes with:~%  ~A ../hashes/checksum.md5 " *cmd-hash-check*) out)
    T)))

;;;; check target has write access

(defun test-ssh-connection ()
  "Test if SSH connection works with verbose output"
  (jrn:framed (test-ssh-connection :log-record (when (>= *log-level* 1) *log*))
    ;; (journal:logged ((when *debug* *log*)) "a: ~A" a)
    (when (not (string-equal "" *remote-login*))
    (let ((host (string-right-trim ":" *remote-login*)))
      (format t "~&Testing SSH connection to: ~A~%" host)
      ;; Add -v for verbose SSH output
      (handler-case
          (uiop:run-program (format nil "ssh -v ~A 'echo SSH_OK'" host)
                            :output :interactive
                            :error-output :interactive)
        (error (e)
          (format t "~&SSH Error: ~A~%" e)
          (format t "~&Try running manually: ssh ~A~%" host)))))
    ))

(defun test-transfer ()

  (jrn:framed (test-transfer :log-record (when (>= *log-level* 1) *log*))
    ;; (journal:logged ((when *debug* *log*)) "a: ~A" a)
    (let* (
           (file (namestring (filepaths:join (make-docs-dir) "compression-docs.txt")))
           (target (format nil "~A~A~A"
                           *remote-login*
                           *target-dir*
                           (filepaths:name file)))
           )

      (journal:logged ((when *debug* *log*)) "target: ~A" target)
      ;; (test-ssh-connection) ; &&& borks
      (format t "~&Copying file:~&  ~A" file)
      (format t "~&Testing target dir write access:~&  ~A" target)
      (call-rsync file target :flags "-vP")
      )))

;;;; file operations, link and hash

(defun make-symlink (file)
  " creates a symlink in payload "
  (jrn:framed (make-symlink :log-record (when (>= *log-level* 1) *log*)
                            :args `(,file))
    (journal:logged ((when *debug* *log*)) "file: ~A" file)
    (format t "~&Linking:~%  '~A'" file)
    (let* (
           (real (namestring
                  (uiop:ensure-absolute-pathname file)))
           (link (namestring
                  (merge-pathnames (filepaths:name file)
                                   (make-payload-dir))))
           )
      (org.shirakumo.filesystem-utils:create-symbolic-link link real))))

(defun link-all ()
  " symlinks all files in payload list into the payload dir "

  (jrn:framed (link-all :log-record (when (>= *log-level* 1) *log*))

    (journal:logged ((when *debug* *log*)) "payload-files: ~A" *payload-files*)
    (mapcar #'make-symlink *payload-files*)))


(defun hash-file (file)
  "hash a file in payload into hashes dir"
  (jrn:framed (hash-file :log-record (when (>= *log-level* 1) *log*)
                         :args `(,file))
    (journal:logged ((when *debug* *log*)) "file: ~A" file)
    (format t "~&Hashing:~%  '~A'" file)
    (cmd:cmd (format nil
                     "md5sum '~A' >> ../hashes/checksum.md5"
                     (filepaths:name file)))))

(defun hash-all ()
  (jrn:framed (hash-all :log-record (when (>= *log-level* 1) *log*))
    (journal:logged ((when *debug* *log*)) "make-payload-dir: ~A" (make-payload-dir))
    (uiop:with-current-directory  ((make-payload-dir))
      (cmd:cmd "echo '# made with lisp' > ../hashes/checksum.md5") ;; clean start
      (mapcar #'hash-file
              (uiop:directory-files (uiop:getcwd))))))

;;;; recursive compress data-transfer dir

(defun compress ()
  "
calls cmd-compress on the transfer dir, creating the new name
"
  (jrn:framed (compress :log-record (when (>= *log-level* 1) *log*))

    (uiop:with-current-directory (*operations-home*)
      (let ((payload-name (pathname-utils:directory-name (make-transfer-dir))))

        (journal:logged ((when *debug* *log*)) "make-compressed-name: ~A" (make-compressed-name))
        (journal:logged ((when *debug* *log*)) "payload-name: ~A" payload-name)
        (cmd:cmd (format nil "~A '~A' '~A'"
                         *cmd-compress* (make-compressed-name) payload-name))))))

;;;; move dir to target
(defun transfer ()
  " rsync the compressed data to target "
  (jrn:framed (transfer :log-record (when (>= *log-level* 1) *log*))

    (let (
          (file (make-compressed-name))
          (target (format nil "~A~A" *remote-login* *target-dir*))
          )

      (journal:logged ((when *debug* *log*)) "file: ~A" file)
      (journal:logged ((when *debug* *log*)) "target: ~A" target)
      (format t "~&Copying file:~&  ~A" file)
      (format t "~&Transferring to target dir:~&  ~A" target)

      (uiop:with-current-directory (*operations-home*)
        (call-rsync file target :flags "-vrPL"))
      )))

;;;; clean up
(defun cleanup ()
  (jrn:framed (cleanup :log-record (when (>= *log-level* 1) *log*))
    (org.shirakumo.filesystem-utils:delete-directory (make-transfer-dir))
    (uiop:with-current-directory (*operations-home*)
      (org.shirakumo.filesystem-utils:delete-file* (make-compressed-name)))))

(defun deliver-payload ()
  "perform all operations to transfer payload"
  (init-mem-log)

  (jrn:framed (deliver-payload :log-record (when (>= *log-level* 1) *log*))
    (journal:logged ((when *debug* *log*)) "begin:deliver payload")
    (create-transfer-directories)
    (create-documents)
    (test-transfer)
    (check-payload)
    (link-all)
    (hash-all)
    (compress)
    (transfer)
    (cleanup)
    (format t "~&~%payload delivered")))

(in-package :payload)

;;;; check dependencies
(defun check-dependencies ()
  (cmd:$cmd "which tar")
  (cmd:$cmd "which rsync")
  (cmd:$cmd "which md5sum")
  ;; all good
  T)

(defparameter *project-name* nil
  "name to prepend &&&")

(defparameter *remote-login* ""
  "string like: user@remote:
with trailing colon or empty string if local")

(defparameter *target-dir* nil
  "an absolute file path to a directory as string with trailing slash")
(defparameter *payload-files* nil
  " a list of file paths (no dirs)")
(defparameter *payload-dirs* nil
  "&&& not implemented list of file paths to dirs (no files))")
(defparameter *operations-home* (user-homedir-pathname)
  "pathname used as toplevel of staging")

(defun check-parameters ()
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
  "dir for &&&"
  (filepaths:join (make-transfer-dir) "hashes/" ))
(defun make-compressed-name ()
  (str:concat (parent-namestring (make-transfer-dir)) ".tar.gz"))

;;;; create dir structure
(defun create-transfer-directories ()
  (check-dependencies)
  (check-parameters)
  (ensure-directories-exist (make-transfer-dir))
  (ensure-directories-exist (make-payload-dir))
  (ensure-directories-exist (make-docs-dir))
  (ensure-directories-exist (make-hashes-dir))
  (ensure-directories-exist (make-transfer-dir)))

;;;; define commands
(defparameter *cmd-compress* "tar -chzvf") ;; -h to derefrence symlinks!
(defparameter *cmd-decompress* "tar -xzvf")
(defparameter *cmd-hash* "md5sum * > ../hashes/checksum.md5")
(defparameter *cmd-hash-check* "md5sum --check")

(defun call-rsync (file target &key (flags "-vrPL --dry-run"))
  "
ARGS:
file: string like /path/to/file.type
target: string like user@remote:/path/to/dest/
flags: string like -vrPL --dry-run
"

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
      )))

;;;; documents
(defun create-documents ()
  " creates simple documents for the end users"
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
    T))

;;;; check target has write access

(defun test-ssh-connection ()
  "Test if SSH connection works with verbose output"
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
          (format t "~&Try running manually: ssh ~A~%" host))))))

(defun test-transfer ()
  (let* (
         (file (namestring (filepaths:join (make-docs-dir) "compression-docs.txt")))
         (target (format nil "~A~A~A"
                         *remote-login*
                         *target-dir*
                         (filepaths:name file)))
         )
    (test-ssh-connection)
    (format t "~&Copying file:~&  ~A" file)
    (format t "~&Testing target dir write access:~&  ~A" target)
    (call-rsync file target :flags "-vP")
    ))

;;;; file operations, link and hash

(defun check-payload-files ()
  "check each file in *payload-files* exists"
  ;; print current state
  (if (null *payload-files*)
      (warn "*payload-files* is nil"))
  (mapcar (lambda (f) (format t "~&file: ~A existsp: ~A~%" f (probe-file f)))
          *payload-files*)

  (assert (every (lambda (f) (not (null (probe-file f))))
                 *payload-files*)
          () "all payload files must exist"))

(defun make-symlink (file)
  " creates a symlink in payload "
  (format t "~&Linking:~%  '~A'" file)
  (let* (
         (real (namestring
             (uiop:ensure-absolute-pathname file)))
         (link (namestring
             (merge-pathnames (filepaths:name file)
                              (make-payload-dir))))
         )
    (org.shirakumo.filesystem-utils:create-symbolic-link link real)))

(defun link-all ()
  " symlinks all files in payload list into the payload dir "
  (mapcar #'make-symlink *payload-files*))

(defun hash-file (file)
  "hash a file in payload into hashes dir"
  (format t "~&Hashing:~%  '~A'" file)
  (cmd:cmd (format nil
                   "md5sum '~A' >> ../hashes/checksum.md5"
                   (filepaths:name file))))

(defun hash-all ()
  (uiop:with-current-directory  ((make-payload-dir))

    (cmd:cmd "echo '# made with lisp' > ../hashes/checksum.md5") ;; clean start
    (mapcar #'hash-file
              (uiop:directory-files (uiop:getcwd)))))

;;;; recursive compress data-transfer dir

(defun compress ()
  "
calls cmd-compress on the transfer dir, creating the new name
"
  (uiop:with-current-directory (*operations-home*)
    (let ((payload-name (pathname-utils:directory-name (make-transfer-dir))))
      (cmd:cmd (format nil "~A '~A' '~A'"
                       *cmd-compress* (make-compressed-name) payload-name)))))

;;;; move dir to target
(defun transfer ()
  " rsync the compressed data to target "
  (let (
        (file (make-compressed-name))
        (target (format nil "~A~A" *remote-login* *target-dir*))
        )

    (format t "~&Copying file:~&  ~A" file)
    (format t "~&Transferring to target dir:~&  ~A" target)

    (uiop:with-current-directory (*operations-home*)
      (call-rsync file target :flags "-vrPL"))
    ))


;;;; clean up
(defun cleanup ()
  (org.shirakumo.filesystem-utils:delete-directory (make-transfer-dir)  )
  (uiop:with-current-directory (*operations-home*)
    (org.shirakumo.filesystem-utils:delete-file* (make-compressed-name))))

(defun deliver-payload ()
  "perform all operations to transfer payload"
  (create-transfer-directories)
  (create-documents)
  (test-transfer)
  (check-payload-files)
  (link-all)
  (hash-all)
  (compress)
  (transfer)
  (cleanup)
  (format t "~&~%payload delivered")
  )

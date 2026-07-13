(asdf:load-system :journal)

(defvar *log* nil)
(defparameter *log-level* 1)
(defparameter *debug* t)

(defun init-log (&key (log-level 1))
  (let* ((stream (open "logging.log"
                       :direction :output
                       :if-does-not-exist :create
                       :if-exists :supersede))
         (journal (journal:make-pprint-journal
                   :stream (make-broadcast-stream
                            (make-synonym-stream '*standard-output*)
                            stream))))
    (setf *log* journal)
    (setf *log-level* log-level)
    (journal:logged (*log*) "~%reinitialized log to log-level ~A" log-level)
    ))

(defun init-file-log (&key (log-level 1))
  (let (
        (journal (journal:make-file-journal "logging.log"))
        )
    (jrn:journal-sync journal)
    (setf *log* journal)
    (setf *log-level* log-level)
    (journal:logged (*log*) "~%reinitialized log to log-level ~A" log-level)
    ))

(defun init-mem-log (&key (log-level 1))
  (let (
        (journal (journal:make-in-memory-journal))
        )
    (setf *log* journal)
    (setf *log-level* log-level)
    (journal:logged (*log*) "~%reinitialized memory log to log-level ~A" log-level)
    ))

(defun fun0 (a)
  (journal:framed (fun0 :log-record *log*
                        :args `(,a))
    (journal:logged ((when *debug* *log*)) "in fun0 ~A" a)
    a))

(defun fun1 (a)
  (journal:framed (fun1 :log-record *log*
                        :args `(,a))
    (journal:logged ((when *debug* *log*)) "in fun1 ~A" a)
    (fun0 "called from fun1")
    a))

;;;;

(init-mem-log)
(defun top ()
  (journal:logged ((when *debug* *log*)) "begin at top")
  (journal:framed(top :log-record *log*)
    (fun0 "called from top")
    (fun1 "called from top")
    ))

;; (top)

;; (journal:pprint-events
;;  (journal:list-events *log*))

;; (journal:jtrace fun0 fun1 )
;; (journal:juntrace fun0 fun1 )

(uiop:define-package #:lisphp
  (:use #:cl)
  (:import-from #:hunchentoot #:acceptor #:acceptor-document-root #:request-pathname)
  (:export #:lisphp-acceptor #:start-server))

(uiop:define-package #:lisphp-user
  (:use #:cl))

(in-package :lisphp)

(defclass lisphp-acceptor (acceptor)
  ()
  (:documentation "Acceptor that serves lisphp files."))

(defmethod hunchentoot:acceptor-dispatch-request ((acceptor lisphp-acceptor) request)
  (let ((path (and (acceptor-document-root acceptor)
		   (request-pathname request))))
    (cond
      ((and path (equal (pathname-type path) "lhp"))
       (serve-lisphp (merge-pathnames path (acceptor-document-root acceptor))))
      (t
       (call-next-method)))))

(defun start-server (&rest args)
  (hunchentoot:start (apply #'make-instance 'lisphp-acceptor args)))

(defun lisphp-package-name (pathname)
  (concatenate 'string "LISPHP-USER-"
	       (namestring (truename pathname))))

(defun compile-lisphp (pathname)
  (with-open-file (stream pathname :direction :input)
    (let* ((package-name (lisphp-package-name pathname))
	   (*package* (or (find-package package-name) (make-package package-name)))
	   (options (read stream))
	   (let-bindings (loop for spec in (first options)
			    collect `(,spec (hunchentoot:parameter ,(string-downcase spec)))))
	   (quickload (getf (rest options) :quickload))
	   (needed-systems (set-difference quickload (asdf:already-loaded-systems) :test #'string=))
	   (use (getf (rest options) :use '("COMMON-LISP"))))
      (when needed-systems
	(ql:quickload needed-systems :silent t))
      (uiop:ensure-package (lisphp-package-name pathname) :use use)
      (let ((body (loop for expr = (read stream nil)
		     while expr collect expr)))
	`(lambda () (let ,let-bindings ,.body))))))

(defparameter *compile-cache* (make-hash-table :test 'equal))

(defun get-compiled-lisphp (pathname)
  (let ((write-date (file-write-date pathname))
	(cache (gethash pathname *compile-cache*)))
    (destructuring-bind (&optional cache-date cache-fn) cache
      (if (and cache-date (= write-date cache-date))
	  cache-fn
	  (let ((new-fn (compile nil (compile-lisphp pathname))))
	    (setf (gethash pathname *compile-cache*) (list write-date new-fn))
	    new-fn)))))

(defun serve-lisphp (pathname)
  (setf (hunchentoot:content-type*) "text/html")
  (let ((fn (get-compiled-lisphp pathname))
	(*standard-output* (flexi-streams:make-flexi-stream (hunchentoot:send-headers))))
    (funcall fn)))
      
		   

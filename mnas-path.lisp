;;;; mnas-path.lisp

(in-package #:mnas-path)

;;; "mnas-path" goes here. Hacks and glory await!

(defun pathname-directory-subtract (path-1 path-2 &key (absolute t))
  (do  ((dir-1  (pathname-directory path-1) (cdr dir-1))
	(dir-2  (pathname-directory path-2) (cdr dir-2))
	(flnm   (pathname-name path-2))
	(ext    (pathname-type path-2))
	)
       ((null dir-1)
	(make-pathname :directory (cons (if absolute :absolute :relative) dir-2) :name flnm :type ext))
    (if (not(equal (car dir-1) (car dir-2)))
	(setf dir-1 nil
	      dir-2 nil))))

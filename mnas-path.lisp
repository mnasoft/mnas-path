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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun walk-file-by-extension (dirname extension 
			      &key 
				(fn #'(lambda(x) (write-line (namestring x))))
				(fn-extension 
				 #'(lambda (x)
				     (string= (pathname-type x) extension)))
				(dir-ignore 
				 #'(lambda (x) (string= (first (last (pathname-directory x))) ".git"))))
  "Пример использования:
;;;; (walk-file-by-extension  \"/_storage/otd11/namatv/develop/git/clisp/\" \"\" :fn-extension #'(lambda (x) (member (pathname-type x) '(\"lisp\" \"txt\") :test #'string=)))
;;;;(walk-file-by-extension \"/_storage/otd11/namatv/develop/git/clisp/\" \"asd\")
"
  (cl-fad:walk-directory dirname
			 #'(lambda (x) 
			     (unless (cl-fad:directory-pathname-p x)
			       (funcall fn x)))
			 :directories :breadth-first 
			 :test #'(lambda (x) 
				   (cond
				     ((funcall dir-ignore x) nil)
				     ((cl-fad:directory-pathname-p x))
				     ((funcall fn-extension x))
				     (t nil)))))

;;;; (walk-file-by-extension "/_storage/otd11/namatv/develop/git/clisp/" "asd")
;;;; (walk-file-by-extension  "/_storage/otd11/namatv/develop/git/clisp/" "" :fn-extension #'(lambda (x) (member (pathname-type x) '("lisp" "txt") :test #'string=)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun find-filename (dirname extension)
"Возвращает список файлов, у которых расширение соответствует extension;
Поиск начинается с каталога dirname, вглубь дерева каталогов;
Елементами возврвщаемого списка являются строки;
Пример использования:
;;;; (find-filename  \"/_storage/otd11/namatv/develop/git/clisp/\"  \"asd\")
"
  (let ((rez nil))
    (walk-file-by-extension 
     dirname
     extension  
     :fn #'(lambda (x) (push (namestring x) rez)))
    (reverse rez)))

;;;; (find-filename  "/_storage/otd11/namatv/develop/git/clisp/"  "asd")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun find-filename-directory (dirname extension)
"Возвращает список каталогов, в которых присутствуют файлы с расширением extension;
Поиск начинается с каталога dirname, вглубь дерева каталогов;
Елементами возврвщаемого списка являются строки;
Пример использования:
;;;; (find-filename-directory \"/_storage/otd11/namatv/develop/git/clisp/\" \"asd\")
"
  (let ((rez nil))
    (walk-file-by-extension 
     dirname
     extension  
     :fn #'(lambda (x) (pushnew (namestring (cl-fad:pathname-directory-pathname x)) rez :test #'string=)))
    (reverse rez)))

;;;; (find-filename-directory "/_storage/otd11/namatv/develop/git/clisp/" "asd")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun walk-directory-by-name (dirname name)
  (cl-fad:walk-directory 
   dirname
   #'(lambda (x) 
       (when (cl-fad:directory-pathname-p x)
	 (write-line (namestring x))))
   :directories t
   :test #'(lambda (x) 
	     (cond
	       ((string= (first (last (pathname-directory x))) name))
	       (t nil)))))

;;;; (walk-directory-by-name "/_storage/otd11/namatv/develop/git/clisp" ".git")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;; (walk-directory-by-name "~/develop/git/clisp" ".git")

;;;; (find-filename  "~/develop/git/clisp"  "asd")

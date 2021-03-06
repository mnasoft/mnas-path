;;;; mnas-path.lisp

(in-package :cl-user)

(defpackage :mnas-path
  (:use #:cl)
  (:export find-directory-parent pathname-directory-subtract
           walk-directory-by-name
           find-filename-directory
           walk-file-by-extension
           find-filename))

;;;; (declaim (optimize (compilation-speed 0) (debug 3) (safety 0) (space 0) (speed 0)))

(setf sb-impl::*default-external-format* :utf8)

(in-package :mnas-path)

(export 'pathname-directory-subtract )

(defun pathname-directory-subtract (path-1 path-2 &key (absolute t))
  "@b(Описание:) функция @b(pathname-directory-subtract) вычитает из пути
path-1 путь path-2.

 @b(Пример использования:)
@begin[lang=lisp](code)
 (pathname-directory-subtract \"~/quicklisp/local-projects/\" 
                              \"~/quicklisp/local-projects/mnas/mnas-path/docs/1.txt\")
 => #P\"/mnas/mnas-path/docs/1.txt\"
@end(code)
"
#| (pathname-directory-subtract "~/quicklisp/local-projects/" 
                                "~/quicklisp/local-projects/mnas/mnas-path/docs/1.txt")
   (pathname-directory-subtract "~/public/" 
                                "~/quicklisp/local-projects/mnas/mnas-path/docs/1.txt")
|#
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

(export 'walk-file-by-extension )

(defun walk-file-by-extension (dirname extension 
			       &key 
				 (fn #'(lambda(x) (write-line (namestring x))))
				 (fn-extension 
				  #'(lambda (x)
				      (string= (pathname-type x) extension)))
				 (dir-ignore 
				  #'(lambda (x) (string= (first (last (pathname-directory x))) ".git"))))
" @b(Пример использования:)
@begin[lang=lisp](code)
 (walk-file-by-extension  \"/_storage/otd11/namatv/develop/git/clisp/\" \"\" :fn-extension #'(lambda (x) (member (pathname-type x) '(\"lisp\" \"txt\") :test #'string=)))
 (walk-file-by-extension \"/_storage/otd11/namatv/develop/git/clisp/\" \"asd\")
@end(code)
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(export 'find-filename )

(defun find-filename (dirname extension)
"@b(Описание:) find-filename возвращает список файлов, у которых 
расширение соответствует extension;

Поиск начинается с каталога dirname, вглубь дерева каталогов;

Элементами возврвщаемого списка являются строки;

 @b(Пример использования:)
@begin[lang=lisp](code)
 (find-filename  \"/_storage/otd11/namatv/develop/git/clisp/\"  \"asd\")
@end(code)
"
  (let ((rez nil))
    (walk-file-by-extension 
     dirname
     extension  
     :fn #'(lambda (x) (push (namestring x) rez)))
    (reverse rez)))

(export 'find-filename-directory )

(defun find-filename-directory (dirname extension)
"@b(Описание:) find-filename-directory возвращает список каталогов, 
в которых присутствуют файлы с расширением extension;

Поиск начинается с каталога dirname, вглубь дерева каталогов;

Елементами возврвщаемого списка являются строки;

@b(Пример использования:)
@begin[lang=lisp](code)
 (find-filename-directory \"/_storage/otd11/namatv/develop/git/clisp/\" \"asd\")
@end(code)
"
  (let ((rez nil))
    (walk-file-by-extension 
     dirname
     extension  
     :fn #'(lambda (x) (pushnew (namestring (cl-fad:pathname-directory-pathname x)) rez :test #'string=)))
    (reverse rez)))

(export 'walk-directory-by-name )

(defun walk-directory-by-name (dirname name &key (fn #'(lambda (x) (write-line (namestring x)))))
"@b(Описание:) walk-directory-by-name"
  (cl-fad:walk-directory 
   dirname
   #'(lambda (x) 
       (when (cl-fad:directory-pathname-p x)
	 (funcall fn x)))
   :directories t
   :test #'(lambda (x) 
	     (cond
	       ((string= (first (last (pathname-directory x))) name))
	       (t nil)))))

(export 'find-directory-parent )

(defun find-directory-parent (dirname name)
"@b(Описание:) find-directory-parent"
  (let ((rez nil))
  (walk-directory-by-name
   dirname
   name
   :fn #'(lambda (x)
	   (push (cl-fad:pathname-parent-directory x) rez)))
    (reverse rez)))

(defpackage #:mnas-path/docs
  (:use #:cl ) 
  (:nicknames "MPATH/DOCS")
  (:export make-all)
  (:documentation "Пакет @b(mnas-path/docs) содержит функции
  генерирования и публикации документации."))

(in-package :mnas-path/docs)

(defun make-document ()
  (loop
    :for i :in
    '((:mnas-path          :mnas-path)
      )
    :do (apply #'mnas-package:document i)))

(defun make-graphs ()
  (loop
    :for i :in
    '(:mnas-path
      )
    :do (mnas-package:make-codex-graphs i i)))

(defun make-all (&aux
                   (of (if (find (uiop:hostname)
                                 mnas-package:*intranet-hosts*
                                 :test #'string=)
                           '(:type :multi-html :template :gamma)
                           '(:type :multi-html :template :minima))))
  "@b(Описание:) функция @b(make-all) служит для создания документации.

 Пакет документации формируется в каталоге
~/public_html/Common-Lisp-Programs/mnas-path.
"
  (mnas-package:make-html-path :mnas-path)
  (make-document)
  (make-graphs)
  (mnas-package:make-mainfest-lisp
   '(:mnas-path :mnas-path/docs)
   "Mnas-Path"
   '("Mykola Matvyeyev")
   (mnas-package:find-sources "mnas-path")
   :output-format of)
  (codex:document :mnas-path)
  (make-graphs)
  (mnas-package:copy-doc->public-html "mnas-path")
  (mnas-package:rsync-doc "mnas-path"))

#+nil
(make-all)

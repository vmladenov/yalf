;;;;-*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-
;;;;
;;;; Basic routines
;;;;
;;;; Copyright (C) 2010 Vesselin Mladenov <veselinm@gmail.com>
;;;;
;;;; You are granted the rights to distribute and use this software
;;;; as governed by the terms of the BSD License
;;;; BSD style license: http://www.opensource.org/licenses/bsd-license.php


(in-package :yalf)

#+:sbcl
(defmacro with-mutex ((mutex) &body body)
  `(sb-thread:with-mutex (,mutex)
     ,@body))

#-:sbcl
(defmacro with-mutex ((mutex) &body body)
  `(progn ,@body))


;;;; Local Variables:
;;;; mode: lisp
;;;; End:

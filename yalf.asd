;;;;-*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-
;;;;
;;;; This is the YALF ASDF definition
;;;;
;;;; Copyright (C) 2010 Vesselin Mladenov <veselinm@gmail.com>
;;;;
;;;; You are granted the rights to distribute and use this software
;;;; as governed by the terms of the BSD License
;;;; BSD style license: http://www.opensource.org/licenses/bsd-license.php

(in-package #:common-lisp-user)

(defpackage #:yalf-system 
  (:use #:asdf #:cl))

(in-package #:yalf-system)

(defsystem yalf
  :version "0.0.2"
  :author "Vesselin Mladenov <veselinm@gmail.com>"
  :maintainer "Vesselin Mladenov <veselinm@gmail.com>"
  :licence "BSD License"
  :description "yet another logging facility"
  :serial t
  :depends-on (:trivial-gray-streams
	       :cl-fad
	       :cl-ppcre
	       :arnesi
	       :local-time)
  :components
  ((:static-file "COPYING")
   (:static-file "README")
   (:module "src"
	    :serial t
	    :components ((:file "package")
			 (:file "basic")
			 (:file "channel")
			 (:file "logger")
			 (:file "yalf")))))


;;;; Local Variables:
;;;; mode: lisp
;;;; End:

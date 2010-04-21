;;;;-*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-
;;;;
;;;; This is the YALF package definition
;;;;
;;;; Copyright (C) 2010 Vesselin Mladenov <veselinm@gmail.com>
;;;;
;;;; You are granted the rights to distribute and use this software
;;;; as governed by the terms of the BSD License
;;;; BSD style license: http://www.opensource.org/licenses/bsd-license.php

(in-package :common-lisp-user)

(defpackage :yalf
  (:use :common-lisp
	:trivial-gray-streams
	:local-time)
  (:export #:channel-open
	   #:channel-close
	   #:attach-severity
	   #:attach-channel
	   #:create-default-logger	  
	   #:log-message
	   #:log-emergency
	   #:log-alert
	   #:log-critical
	   #:log-error
	   #:log-warning
	   #:log-info
	   #:log-notice
	   #:log-debug
	   #:log-trace
	   #:+LOG-EMERGENCY+
	   #:+LOG-ALERT+
	   #:+LOG-CRITICAL+
	   #:+LOG-ERROR+
	   #:+LOG-WARNING+
	   #:+LOG-NOTICE+
	   #:+LOG-INFO+
	   #:+LOG-DEBUG+
	   #:+LOG-TRACE+
	   #:*logger*)
  (:documentation "Yet Another Logging Facility"))



(in-package :yalf)



(defgeneric stream-close (obj &key &allow-other-keys))
(defgeneric log-message (logger level message &key &allow-other-keys))

(defgeneric channel-close (channel &key &allow-other-keys))
(defgeneric channel-write (channel message &key &allow-other-keys))
(defgeneric channel-open (channel &key &allow-other-keys))
(defgeneric channel-flush (channel &key &allow-other-keys))

(defgeneric attach-channel (logger channel &key &allow-other-keys))
(defgeneric attach-severity (logger &key &allow-other-keys))


;;;; Local Variables:
;;;; mode: lisp
;;;; End:

;;;;-*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-
;;;;
;;;; This is the man YALF file
;;;;
;;;; Copyright (C) 2010 Vesselin Mladenov <veselinm@gmail.com>
;;;;
;;;; You are granted the rights to distribute and use this software
;;;; as governed by the terms of the BSD License
;;;; BSD style license: http://www.opensource.org/licenses/bsd-license.php

(in-package :yalf)


;;;
;;; Log levels
;;;
(defvar +LOG-EMERGENCY+ 0)
(defvar +LOG-ALERT+ 1)
(defvar +LOG-CRITICAL+ 2)
(defvar +LOG-ERROR+ 3)
(defvar +LOG-WARNING+ 4)
(defvar +LOG-NOTICE+ 5)
(defvar +LOG-INFO+ 6)
(defvar +LOG-DEBUG+ 7)
(defvar +LOG-TRACE+ 8)



(defun create-default-logger (log-file-name logger-name)
  (let ((logger
	 (make-instance 'yalf-logger :name logger-name))
	(channel
	 (make-instance 'yalf-channel-file 
			:name "default channel" 
			:basename log-file-name
			:synchronized t)))
    (attach-channel logger channel :index 0)
    (attach-severity logger :name "emergency" :level 0 :channel-index 0 :long-name "System is unusable")
    (attach-severity logger :name "alert"     :level 1 :channel-index 0 :long-name "Action must be taken immediately")
    (attach-severity logger :name "critical"  :level 2 :channel-index 0 :long-name "Critical conditions")
    (attach-severity logger :name "error"     :level 3 :channel-index 0 :long-name "Error conditions")
    (attach-severity logger :name "warning"   :level 4 :channel-index 0 :long-name "Warning conditions")
    (attach-severity logger :name "notice"    :level 5 :channel-index 0 :long-name "Normal, but significant, condition")
    (attach-severity logger :name "info"      :level 6 :channel-index 0 :long-name "Informational message")
    (attach-severity logger :name "debug"     :level 7 :channel-index 0 :long-name "Debug level message")
    (attach-severity logger :name "trace"     :level 8 :channel-index 0 :long-name "Trace level message")
    logger))


(declaim (inline log-emergency))
(defun log-emergency(&rest rest)
  (log-message *logger* +LOG-EMERGENCY+ (format nil "~{~A~}" rest))
  (values))

(declaim (inline log-alert))
(defun log-alert(&rest rest)
  (log-message *logger* +LOG-ALERT+ (format nil "~{~A~}" rest))
  (values))

(declaim (inline log-critical))
(defun log-critical(&rest rest)
  (log-message *logger* +LOG-CRITICAL+ (format nil "~{~A~}" rest))
  (values))

(declaim (inline log-error))
(defun log-error(&rest rest)
  (log-message *logger* +LOG-ERROR+ (format nil "~{~A~}" rest))
  (values))

(declaim (inline log-warning))
(defun log-warning(&rest rest)
  (log-message *logger* +LOG-WARNING+ (format nil "~{~A~}" rest))
  (values))

(declaim (inline log-info))
(defun log-info(&rest rest)
  (log-message *logger* +LOG-INFO+ (format nil "~{~A~}" rest))
  (values))

(declaim (inline log-notice))
(defun log-notice(&rest rest)
  (log-message *logger* +LOG-NOTICE+ (format nil "~{~A~}" rest))
  (values))

(declaim (inline log-debug))
(defun log-debug(&rest rest)
  (log-message *logger* +LOG-DEBUG+ (format nil "~{~A~}" rest))
  (values))

(declaim (inline log-trace))
(defun log-trace(&rest rest)
  (log-message *logger* +LOG-TRACE+ (format nil "~{~A~}" rest))
  (values))








;;;; Local Variables:
;;;; mode: lisp
;;;; End:

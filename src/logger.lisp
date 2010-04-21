;;;;-*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-
;;;;
;;;; This is the logger definitions and management file
;;;;
;;;; Copyright (C) 2010 Vesselin Mladenov <veselinm@gmail.com>
;;;;
;;;; You are granted the rights to distribute and use this software
;;;; as governed by the terms of the BSD License
;;;; BSD style license: http://www.opensource.org/licenses/bsd-license.php

(in-package :yalf)

(defvar *logger* nil)

#+:sbcl
(defvar *timestamp-printer-mutex* 
  (sb-thread:make-mutex :name "*timestamp-printer-mutex*"))

#-:sbcl 
(defvar *timestamp-printer-mutex* nil)

(defvar *timestamp-printer* nil)

(defun timestamp-printer (&optional printer)
  (let (oldone)
    (with-mutex (*timestamp-printer-mutex*)
      (setq oldone *timestamp-printer*)
      (when printer
	(setq *timestamp-printer* printer)))
    oldone))


(defstruct severity level name channel-index long-name)

(defclass yalf-logger ()
  ((name
    :reader name
    :initarg :name
    :initform (error "Logger name must be supplied"))
   (severities
    :accessor severities
    :initform (make-array 0 :element-type 'cons :adjustable t))
   (max-severity
    :accessor max-severity
    :initarg :max-severity
    :initform -1)
   (mutex
    :reader mutex
    :initform nil)
   (channels
    :accessor channels
    :initform (make-array 0 :element-type 'yalf-channel :adjustable t))))

#+:sbcl
(defmethod initialize-instance :after ((logger yalf-logger) &key (synchronized t))
  (when synchronized
    (setf (slot-value logger 'mutex) (sb-thread:make-mutex))))

(defmethod attach-channel ((logger yalf-logger) channel &key index)
  (with-slots (channels severities mutex) logger
    (with-mutex (mutex)
      (when (not index)
	(setq index (+ (length channels) 1)))
      (when (>= index (length channels))
	(adjust-array channels (+ index 1)))
      (setf (aref channels index) channel)))
  (values))

(defmethod attach-severity ((logger yalf-logger) &key name level channel-index long-name)
  (with-slots (channels severities mutex) logger
    (with-mutex (mutex)
      (when (>= level (length severities))
	(adjust-array severities (+ level 1)))
      (setf (aref severities level)
	    (make-severity :name name 
			   :level level 
			   :channel-index channel-index
			   :long-name long-name))))
  (values))

(defmethod channel-open ((logger yalf-logger) &key)
  (with-slots (channels mutex) logger
    (with-mutex (mutex)
      (loop for channel being the elements of channels
	 do (channel-open channel))))
  (values))

(defmethod channel-close ((logger yalf-logger) &key)
  (with-slots (channels mutex) logger
    (with-mutex (mutex)
      (loop for channel being the elements of channels
	 do (channel-close channel))))
  (values))

;; (format nil "~A [~9<~A~>]: ~A" 
(defmethod log-message ((logger yalf-logger) level message &key)
  (with-slots (channels severities max-severity mutex) logger
    (flet ((make-log-message ()
	     (format nil "~A [~A] ~A" 
		     (funcall (timestamp-printer))
		     (severity-name (aref severities level))
		     message)))
      (with-mutex (mutex)
	(channel-write 
	 (aref channels (severity-channel-index (aref severities level))) 
	 (make-log-message)))
      (values))))





;;;; Local Variables:
;;;; mode: lisp
;;;; End:

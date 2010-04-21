;;;;-*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-
;;;;
;;;; This is the YALF logging channels definition
;;;;
;;;; Copyright (C) 2010 Vesselin Mladenov <veselinm@gmail.com>
;;;;
;;;; You are granted the rights to distribute and use this software
;;;; as governed by the terms of the BSD License
;;;; BSD style license: http://www.opensource.org/licenses/bsd-license.php


(in-package :yalf)


(defclass yalf-channel ()
  ((name
    :reader name
    :initarg :name
    :initform (error "Channel named must be supplied"))
   (byte-counter
    :reader byte-counter
    :initform 0)
   (message-counter
    :reader message-counter
    :initform 0)
   (mutex
    :reader mutex
    :initform nil)
   (sink
    :initform '())
   (sink-max-size
    :accessor sink-max-size
    :initarg :sink-max-size
    :initform -1)))

#+:sbcl
(defmethod initialize-instance :after ((channel yalf-channel) &key (synchronized t))
  (when synchronized
    (setf (slot-value channel 'mutex) (sb-thread:make-mutex))))


(defmethod channel-write ((channel yalf-channel) message &key)
  (with-slots (sink sink-max-size message-counter byte-counter mutex) channel
    (with-mutex (mutex)
      (when (and (>= (length sink) sink-max-size)
		 (> sink-max-size 0))
	(setq sink (butlast sink)))
      (push message sink)
      (incf byte-counter (length message))
      (incf message-counter)
      (values))))

(defmethod channel-close ((channel yalf-channel) &key)
  (with-slots (mutex sink) channel
    (with-mutex (mutex)
      (setf sink '())))
  (values))

(defmethod channel-open ((channel yalf-channel) &key)
  (with-slots (mutex sink) channel
    (with-mutex (mutex)
      (setf sink '())))
  (values))

(defmethod channel-flush  ((channel yalf-channel) &key)
  (with-slots (mutex sink) channel
    (with-mutex (mutex)
      (setf sink '())))
  (values))


(defclass yalf-channel-file (yalf-channel trivial-gray-stream-mixin)
  ((stream 
    :reader stream-of
    :initarg :stream)
   (basename
    :reader basename
    :initarg :basename
    :initform (error "Channel's basename must be supplied"))
   (sync-p
    :accessor sync-p
    :initarg :sync-p
    :initform t)
   (arch-files-num
    :accessor arch-files-num
    :initarg :arch-files-num
    :initform -1)))

(defmethod stream-close ((channel yalf-channel-file) &key abort)
  (with-slots (stream) channel
    (close stream :abort abort))
  (values))

(defmethod stream-write-sequence ((channel yalf-channel-file) seq start end &key)
  (with-slots (stream) channel
    (write-sequence seq stream :start start :end end))
  (values))

(defmethod channel-close ((channel yalf-channel-file) &key abort)
  (with-slots (mutex) channel
    (with-mutex (mutex)
      (stream-close channel :abort abort)))
  (values))

(defmethod channel-open ((channel yalf-channel-file) &key)
  (with-slots (mutex basename stream) channel
    (with-mutex (mutex)
      (setq stream 
	    (open basename 
		  :direction :output
		  :if-exists :append
		  :if-does-not-exist :create))))
  (values))


(defmethod channel-write ((channel yalf-channel-file) message &key)
  (with-slots (stream message-counter byte-counter mutex sync-p) channel
    (with-mutex (mutex)
      (stream-write-sequence channel message 0 (length message))
      (when (fresh-line stream)
	(incf byte-counter))
      (when sync-p
	(finish-output stream))
      (incf byte-counter (length message))
      (incf message-counter)))
  (values))


;;;; Local Variables:
;;;; mode: lisp
;;;; End:

;;;;-*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-
;;;;
;;;; This is the test file for the YALF
;;;;
;;;; Copyright (C) 2010 Vesselin Mladenov <veselinm@gmail.com>
;;;;
;;;; You are granted the rights to distribute and use this software
;;;; as governed by the terms of the BSD License
;;;; BSD style license: http://www.opensource.org/licenses/bsd-license.php

(in-package :common-lisp-user)



(asdf:operate 'asdf:load-op :swank)
(asdf:operate 'asdf:load-op :yalf)

(in-package :yalf)

(let ((*logger* 
       (create-default-logger "/tmp/yalf.log" "default logger")))
  (unwind-protect
       (progn
	 (channel-open *logger*)
	 (log-message *logger* +LOG-ERROR+ "AAAAAAAAAAAAAAAAAAA")
	 (log-message *logger* +LOG-ERROR+ "Beee maaaani")
	 (log-notice "AAA"))
    (channel-close *logger*)))


(sb-ext:quit)

;;;; Local Variables:
;;;; mode: lisp
;;;; End:

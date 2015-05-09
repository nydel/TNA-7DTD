;;;; -*- Mode: Lisp; -*-

(defpackage :tna-7dtd
  (:nicknames :tna)
  (:use :cl :bordeaux-threads :cl-ppcre :telnetlib)
  (:export :+init+
	   :+unhook+
	   :*master-listener-handler-thread*
	   :*master-telnet-connection*
	   :*query-telnet-connection*
	   :*server-address*
	   :*server-chat-tag*
	   :*server-telnet-port*
	   :*tna-commands*
	   :gmsg.com/dice
	   :gmsg-p
	   :listener-handler
	   :parse-gmsg-line
	   :parse-telnet-line
	   :process-gmsg
	   :query-connection
	   :server-format
	   :server-say
	   :server-say-format
	   :tn-query
	   :thread-listener))

(in-package :tna-7dtd)

;(defparameter *server-address* #(127 0 0 1))
;(defparameter *server-telnet-port* 8081)
;(defparameter *server-chat-tag* "[TNA]")

(defparameter *server-address* "nydel-700-147c")
(defparameter *server-telnet-port* 25081)
(defparameter *server-chat-tag* "[TNA-2.0.11.4.101A]")

(defvar *master-telnet-connection* nil)
(defvar *master-listener-handler-thread* nil)
(defvar *query-telnet-connection* nil)

(defparameter *tna-commands* '(("//dice" . gmsg.com/dice)
			       ("//loc" . gmsg.query.com/loc)))

(defun server-format (tn &rest format-args)
  (write-ln tn (eval (append (list 'format nil) format-args))))

(defun server-say (string &optional (tn *master-telnet-connection*))
  (server-format tn "say ~C~a~a~C" #\" *server-chat-tag* string #\"))

(defun server-say-format (&rest format-args)
  (server-say (eval (append (list 'format nil) format-args))))

(defun gmsg.com/dice (tn name &rest arg)
  (declare (ignore arg tn))
  (let ((die1 (1+ (random 6)))
	(die2 (1+ (random 6))))
    (server-say-format "~a rolled a ~d and a ~d." name die1 die2)))

(defun process-gmsg (gmsg tn)
  (let* ((msg-split (ppcre:split "\\s" (cadr gmsg)))
	 (gmsga (assoc (car msg-split) *tna-commands* :test #'string-equal)))
    (when gmsga
      (eval (append (list (cdr gmsga) tn (car gmsg)) (cdr msg-split))))))

(defun parse-gmsg-line (gmsg-line)
  (register-groups-bind (name msg)
      ("GMSG:\\s([^:]*):\\s+(.*)" gmsg-line)
    (list name msg)))

(defun gmsg-p (line)
  (all-matches "(GMSG:\\s)([^:]*)(:\\s)(.*)" line))

(defun parse-telnet-line (line tn)
  (when (gmsg-p line)
    (process-gmsg (parse-gmsg-line line) tn)))

(defun listener-handler (tn)
  (let* ((raw-data (read-available-data tn))
	 (data-lines (split "\\n" raw-data)))
    (loop for line in data-lines
	 collect (parse-telnet-line line tn))))

(defun thread-listener (tn)
  (make-thread
   (lambda ()
     (loop do
	  (listener-handler tn)
	  (sleep 0.1)))
   :name (format nil "tna-7dtd listener ~d" (get-universal-time))))

(defun query-connection (telnet-password)
  (let ((query-tn
	 (open-telnet-session *server-address* *server-telnet-port*)))
    (set-telnet-session-option query-tn :remove-return-char t)
    (read-available-data query-tn)
    (write-ln query-tn telnet-password)
    (read-available-data query-tn)
    (write-ln query-tn "loglevel ALL false")
    (read-available-data query-tn)
    (setf *query-telnet-connection* query-tn)))

(defun tn-query (query)
  (server-format *query-telnet-connection* query)
  (sleep 0.25)
  (read-available-data *query-telnet-connection*))

(defun parse-lp-line-for-location (line)
  (let ((loc (register-groups-bind (x y z)
		 ("pos=\\(([^,]+)\\,\\s([^,]+)\\,\\s([^\\)]+)\\)*" line)
	       (list x y z))))
    loc))

(defun gmsg.query.com/loc (tn name &rest arg)
  (declare (ignore tn arg))
  (let* ((lp-results (tn-query "lp"))
	 (lp-split (split "\n" lp-results))
	 (lp-entry (remove-if-not (lambda (y) (search name y)) lp-split))
	 (loc (parse-lp-line-for-location (car lp-entry))))
    (server-say-format "x: ~a, y: ~a, z: ~a" (first loc) (second loc) (third loc))))
    
(defun +init+ (telnet-password)
  (query-connection telnet-password)
  (let ((master-tn
	 (open-telnet-session *server-address* *server-telnet-port*)))
    (set-telnet-session-option master-tn :remove-return-char t)
    (read-available-data master-tn)
    (write-ln master-tn telnet-password)
    (read-available-data master-tn)
    (write-ln master-tn "loglevel ALL false")
    (read-available-data master-tn)
    (write-ln master-tn "loglevel INF true")
    (read-available-data master-tn)
    (setf *master-telnet-connection* master-tn))
    (read-available-data *master-telnet-connection*)
    (thread-listener *master-telnet-connection*))

(defun +unhook+ ()
  (close-telnet-session *master-telnet-connection*)
  (close-telnet-session *query-telnet-connection*))

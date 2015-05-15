;;;; -*- Mode: Lisp; -*-

(ql:quickload '(:bordeaux-threads :cl-irc :cl-ppcre :telnetlib))

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
			       ("//loc" . gmsg.query.com/loc)
			       ("//wallet" . gmsg.query.com/wallet)
			       ("//shop" . gmsg.query.com/shop)
			       ("//buy" . gmsg.query.com/buy)
			       ("//pay" . gmsg.com/pay)
			       ("//minutes" . gmsg.query.com/minutes)
			       ("//zombies" . gmsg.query.com/zombies)))

(defun server-format (tn &rest format-args)
  (write-ln tn (eval (append (list 'format nil) format-args))))

(defun server-say (string &optional (tn *master-telnet-connection*))
  (server-format tn "say ~C~a~a~C" #\" *server-chat-tag* string #\"))

(defun server-pm (name string &optional (tn *master-telnet-connection*))
  (server-format tn "pm ~a ~C~a~a~C" (get-id-from-name name) #\" *server-chat-tag* string #\"))

(defun server-say-format (&rest format-args)
  (server-say (eval (append (list 'format nil) format-args))))

(defun clear-chat ()
  (loop for i from 0 to 81 do
       (server-say "----------clearing chat...----------")))

(defun gmsg.com/dice (tn name &rest arg)
  (declare (ignore arg tn))
  (let ((die1 (1+ (random 6)))
	(die2 (1+ (random 6))))
    (server-say-format "~a rolled a ~d and a ~d." name die1 die2)))

(defun process-gmsg (gmsg tn)
  (let* ((msg-split (ppcre:split "\\s" (cadr gmsg)))
	 (gmsga (assoc (car msg-split) *tna-commands* :test #'string-equal)))
    (if gmsga
	(eval (append (list (cdr gmsga) tn (car gmsg)) (cdr msg-split)))
	(when *irc-on*
	  (irc:privmsg *irc-connection* *irc-channel*
		       (format nil "[~a] ~a" (car gmsg) (cadr gmsg)))))))

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
	 (lp-split (split "\\n" lp-results))
	 (lp-entry (remove-if-not (lambda (y) (search name y)) lp-split))
	 (loc (parse-lp-line-for-location (car lp-entry))))
    (server-say-format "~a is at x: ~a, y: ~a, z: ~a" name (first loc) (second loc) (third loc))))

(defun gmsg.query.com/minutes (tn name &rest arg)
  (declare (ignore tn arg))
  (let* ((lkp-results (tn-query "lkp"))
	 (lkp-split (split "\\n" lkp-results))
	 (lkp-entry (remove-if-not (lambda (y) (search name y)) lkp-split))
	 (id-playtime
	  (register-groups-bind
	      (id playtime)
	      ("id=([0-9]+).*playtime=([0-9]+)" (car lkp-entry))
	    (list id playtime)
    (server-say-format "~a/~a has played for ~a minutes" name id playtime))))))

(defun gmsg.query.com/zombies (tn name &rest arg)
  (declare (ignore tn arg))
  (let* ((lp-results (tn-query "lp"))
	 (lp-split (split "\\n" lp-results))
	 (lp-entry (remove-if-not (lambda (y) (search name y)) lp-split))
	 (id-zombies-deaths
	  (register-groups-bind
	      (id deaths zombies)
	      ("id=([0-9]+).*deaths=([0-9]+).*zombies=([0-9]+).*" (car lp-entry))
	    (list id zombies deaths)
    (server-say-format "~a/~a has killed ~a zombies and died ~a times" name id zombies deaths))))))


(defun parse-lp-line (line)
  (register-groups-bind
      (id name pos rot remote health deaths zombies players score level steamid ip ping)
      ("^[0-9]+\\.\\s+id=([0-9]+)\\,\\s([^,]+)\\,\\spos=(.+)\\,\\s+rot=(.+)\\,\\s+remote=(.+)\\,\\s+\\health=([0-9]+)\\,\\s+deaths=([0-9]+)\\,\\s+zombies=([0-9]+)\\,\\s+players=([0-9]+)\\,\\s+score=([0-9]+)\\,\\s+level=([0-9]+)\\,\\s+steamid=([0-9]+)\\,\\s+ip=([0-9|\\.]+)\\,\\s+ping=([0-9]+)$"
       line)
    (list id
	  name
	  pos
	  rot
	  remote
	  health
	  deaths
	  zombies
	  players
	  score
	  level
	  steamid
	  ip
	  ping)))

(defun parse-lkp-line (line)
  (register-groups-bind
      (name id steamid online ip playtime seen)
      ("^[0-9]+\\.\\s+(.+)\\,\\s+id=([0-9]+)\\,\\s+steamid=([0-9]+)\\,\\s+online=(.+)\\,\\s+ip=([0-9|\\.]+)\\,\\s+playtime=([0-9]+)\\s+m\\,\\s+seen=([0-9|\\-|\\s|\\:]+)$"
       line)
    (list name id steamid online ip playtime seen)))

(defun lookup-player-lp (name)
  (let* ((lp-results (tn-query "lp"))
	 (lp-lines (split "\\n" lp-results))
	 (lp-line (remove-if-not (lambda (y) (search name y)) lp-lines))
	 (lp-entry (car lp-line)))
	 (parse-lp-line lp-entry)))

(defun lookup-player-lkp (name)
  (let* ((lkp-results (tn-query "lkp"))
	 (lkp-lines (split "\\n" lkp-results))
	 (lkp-line (remove-if-not (lambda (y) (search name y)) lkp-lines))
	 (lkp-entry (car lkp-line)))
	 (parse-lkp-line lkp-entry)))

(defun get-id-from-name (name)
  (let ((lp-results (lookup-player-lp name)))
    (when lp-results (first lp-results))))

(defparameter *coins-to-begin* 50000.0)
(defparameter *coins-per-minute* 50.0)
(defparameter *coins-per-zombie* 75.0)
(defparameter *coins-per-death* -1000.0)
(defparameter *coins-per-use-of-/home* -25000.0)
(defparameter *coins-per-use-of-/sethome* -75000.0)

(defparameter *shop-items* '(("bandage" "firstAidBandage" 25)
			     ("mininghelmet" "miningHelmet" 100)
			     ("splint" "splint" 10)
			     ("medkit" "firstAidKit" 250)
			     ("grainalcohol" "grainAlcohol" 50)
			     ("vegetablestew" "vegetableStew" 35)
			     ("forgeahead" "forgeAheadBook" 500)
			     ("supplycrate" "specialcase1" 1000)
			     ("oilbarrel" "oilBarrel" 350)
			     ("misosoup" "canMiso" 40)
			     ("blip" "stick" -1000)
			     ("repairkit" "weaponRepairKit" 10)
			     ("antibiotics" "antibiotics" 250)))

(defun gmsg.query.com/shop (tn name &rest args)
  (declare (ignore tn name args))
  (mapcar (lambda (y)
	    (server-say-format "item: ~a cost: ~d coins" (first y) (third y)))
	  *shop-items*))

(defvar *player-transactions* nil)

(defun &load-player-transactions ()
  (with-open-file (db #P"player-transactions"
		      :direction :input
		      :if-does-not-exist :create)
    (loop for line = (read-line db nil 'eof nil)
       until (equal line 'eof)
       collect line)))

(defun load-player-transactions ()
  (setf *player-transactions* (&load-player-transactions)))

;(defun add-player-transaction (player-name coin-value note)
;  (let ((player-entry
;	 (remove-if-not (lambda (y) (string-equal (car y) player-name)) *player-transactions*))
;	(other-entries
;	 (remove-if (lambda (y) (string-equal (car y) player-name)) *player-transactions*)))
;    (if player-entry
;	(setf *player-transactions*
;	      (push (push (list coin-value note) player-entry) other-entries))
;	(setf *player-transactions*
;	      (push (list player-name (list coin-value note)))))))
		      

(defun calculate-base-coins (name)
  (let* ((player-lp (lookup-player-lp name))
	 (player-lkp (lookup-player-lkp name))
	 (player-minutes-zombies-deaths (list (sixth player-lkp)
					      (eighth player-lp)
					      (seventh player-lp)))
	 (info (mapcar
		(lambda (y)
		  (read-from-string y)) player-minutes-zombies-deaths)))
    (/ (+ *coins-to-begin*
       (* *coins-per-minute* (first info))
       (* *coins-per-zombie* (second info))
       (* *coins-per-death* (third info)))
       100)))

(defun get-player-transactions (player-name)
  (let ((player-entry (assoc player-name *player-transactions* :test #'string-equal)))
    (when player-entry (cdr player-entry))))

(defun &add-player-transaction (player-name coin-value note)
  (let ((player-transactions (get-player-transactions player-name)))
    (cons player-name (append (list (cons coin-value note)) player-transactions))))

(defun add-player-transaction (player-name coin-value note)
  (let ((other-players
	 (remove-if
	  (lambda (y) (string-equal (car y) player-name))
	  *player-transactions*))
	(this-player
	 (&add-player-transaction player-name coin-value note)))
    (setf *player-transactions*
	  (push this-player other-players))))

(defun gmsg.com/pay (tn payer &rest arg)
  (declare (ignore tn))
  (let* ((arg-split (split "\\s" (car arg)))
	 (receiver (car arg-split))
	 (coin-value (read-from-string (car (last arg-split)))))
    (add-player-transaction payer (* -1 coin-value) (format nil "to ~a" receiver))
    (add-player-transaction receiver coin-value (format nil "from ~a" payer))
    (server-say-format "ok ~a, you paid ~d coins to ~a." payer coin-value receiver)))

(defun calculate-modified-coins (player-name)
  (let ((base-coins (calculate-base-coins player-name))
	(transactions (get-player-transactions player-name))
	(transaction-values (list 0)))
    (when transactions
      (setq transaction-values (mapcar (lambda (y) (car y)) transactions)))
    (eval (append (list '+ base-coins) transaction-values))))

(defun gmsg.query.com/buy (tn name &rest arg)
  (let ((item (assoc (car arg) *shop-items* :test #'string-equal))
	(coins-modified (calculate-modified-coins name)))
    (when item
      (progn
	(when (> (third item) coins-modified)
	  (return-from gmsg.query.com/buy
	    (server-say-format "sorry, ~a, you have insufficient funds." name)))
	(add-player-transaction name (* -1 (third item)) (first item))
	(if (string-equal (second item) "specialcase1")
	    (server-format tn "se ~a ~a" (get-id-from-name name) "31")
	    (server-format tn "give ~a ~a 1" (get-id-from-name name) (second item)))
	(server-say-format "ok ~a, you've purchased a ~a for ~d coins." name (first item) (third item))))))

(defun gmsg.query.com/wallet (tn name &rest args)
  (declare (ignore tn args))
  (server-say-format "~a, you have ~d TNA coins in your wallet."
		     name
		     (calculate-modified-coins name)))

(defvar *dayvote* nil)
(defvar *nightvote* nil)

;(defun gmsg.query.com/timevote (tn name &rest arg)
;  (declare (ignore tn))
;  (cond ((string-equal (car arg) "day")
;	 (

(defparameter *irc-botname* "tnabot")
(defparameter *irc-server* "irc.gamesurge.net")
(defparameter *irc-channel* "#tna")

(defvar *irc-connection* nil)

(defvar *irc-on* nil)

(defun start-irc ()
  (setf *irc-connection* (irc:connect :nickname *irc-botname*
				      :server *irc-server*))
;  (irc:read-message-loop *irc-connection*)
;  (sleep 5.0)
  (make-thread
   (lambda ()
     (irc:read-message-loop *irc-connection*))
     :name (format nil "tna irc thread at ~d" (get-universal-time)))
  (sleep 2)
  (irc:join *irc-connection* "#tna")
  (setf *irc-on* t))

(defun irc-off ()
  (setf *irc-on* nil))
    
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

(ql:quickload '(:bordeaux-threads :cl-irc))

(defvar irc-con nil)

(defun foo ()
  (setf irc-con (cl-irc:connect :nickname "tnabot" :server "irc.gamesurge.net"))
  (bt:make-thread (lambda ()
		    (cl-irc:read-message-loop irc-con)))
  (sleep 2.0)
  (cl-irc:join irc-con "#tna")

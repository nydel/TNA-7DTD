:TNA-7DTD Common Lisp Source

In order to run this while version alpha 2.0.11.4.101 is being finalized,
simply run "main.lisp" through your interpreter - having quicklisp installed
in your REPL, and Alloc's Server Tools running on your server. Set the
appropriate variables such as *server-location* and *server-telnet-port*
etcetera to prepare for launching. Then simply evaluate this form:

(tna::+init+ "serverTelnetPassword")

Everything will load up and save automatically. If anything goes wrong, such
as a server crash or some error inside Lisp, just evaluate:

(tna::+unhook+)

You can then run #'+init+ again as soon as the server is able to accept.
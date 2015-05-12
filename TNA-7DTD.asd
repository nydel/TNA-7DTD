;;;; -*- Mode: Lisp; -*-
;;;; $Id: TNA-7DTD.asd 2.0.11.4.101 2015-05-08 16:59:30 PDT nydel $
;;;; $URL https://github.com/nydel/TNA-7DTD/blob/master/TNA-7DTD.asd $
;;;; $HOMEPADE http://tna.thegrapevine.cc $

;;;; See the LICENSE file for licensing information.

(in-package :cl-user)

(defpackage :tna-asd
  (:use :cl :asdf))

(in-package :tna-asd)

(defsystem :tna
    :serial t
    :version "2.0.11.4.101"
    :author "<nydel@ma.sdf.org>"
    :license "LLGPL v3.0 Modified"
    :description "This package is a Common Lisp implementation of the TNA 1.0
    Specification for use with 7 Days To Die Dedicated Server featuring Alloc's
    open-source Server Fixes. The so-called TNA's Not API Technological Standard
    is proprietary to Joshua Ryan Nydel who holds the Spec's copyright in and
    throughout this universe and all others. The TNA Specification may not be
    used without its proprieter's consent for any purposes. However, the source
    code for distributed implementations of TNA - as in this package, TNA for
    7 Days To Die - is considered free, open-source and for fair use, as further
    defined in this package's LICENSE file."
    :depends-on (:bordeaux-threads
		 :cl-irc
		 :cl-ppcre
		 :telnetlib)
    :components ((:module "data"
		  :serial t
		  :components ((:file "credentials")))
		 (:module "src"
		  :serial t
		  :components ((:file "main")))))

;;; rudel-protocol.el --- Interface implemented by Rudel protocol backends
;;
;; Copyright (C) 2009 Jan Moringen
;;
;; Author: Jan Moringen <scymtym@users.sourceforge.net>
;; Keywords: Rudel, backend, protocol
;; X-RCS: $Id:$
;;
;; This file is part of Rudel.
;;
;; Rudel is free software: you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; Rudel is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with Rudel. If not, see <http://www.gnu.org/licenses>.


;;; Commentary:
;;
;; This file contains the interface for Rudel protocol backends.  This
;; interface consists of the following functions:
;;
;; + joining sessions
;;   + `rudel-ask-join-info'
;;   + `rudel-join'
;; + hosting sessions
;;   + `rudel-ask-host-info'
;;   + `rudel-host'
;; + factory functions
;;   + `rudel-make-document'


;;; History:
;;
;; 0.1 - Initial revision


;;; Code:
;;

(require 'eieio)

(require 'rudel-backend)


;;; Class rudel-protocol-backend
;;

(defclass rudel-protocol-backend (rudel-backend)
  ()
  "Interface implemented by protocol backends."
  :abstract t)

(defgeneric rudel-ask-join-info ((this rudel-protocol-backend))
  "Retrieve information for joining a session from user.
Return a property list that contains the collected information.")

(defgeneric rudel-join ((this rudel-protocol-backend) info)
  "Create a new connection according to the data in the property list INFO.
Implementations can rely on the fact that the property :session
contains the `rudel-session' object to which the new connection
will be associated.")

(defgeneric rudel-ask-host-info ((this rudel-protocol-backend))
  "Retrieve information for hosting a session from user.
Return a property list that contains the collected information.")

(defgeneric rudel-host ((this rudel-protocol-backend) info)
  "Create a new session according to the property list INFO.")

(defgeneric rudel-make-document ((this rudel-protocol-backend)
				 name session)
  "Create a new document object named NAME for SESSION.")

(provide 'rudel-protocol)
;;; rudel-protocol.el ends here

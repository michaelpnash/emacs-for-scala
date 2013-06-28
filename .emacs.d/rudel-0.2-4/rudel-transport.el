;;; rudel-transport.el --- Rudel transport interface and backend
;;
;; Copyright (C) 2009 Jan Moringen
;;
;; Author: Jan Moringen <scymtym@users.sourceforge.net>
;; Keywords: Rudel, backend, transport
;; X-RCS: $Id:$
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <http://www.gnu.org/licenses>.


;;; Commentary:
;;
;; This file contains the interface for Rudel transport backends.


;;; History:
;;
;; 0.1 - Initial revision


;;; Code:
;;

(require 'eieio)

(require 'rudel-backend)


;;; Class rudel-transport
;;

(defclass rudel-transport ()
  ()
  "Interface for transport objects.")

(defgeneric rudel-transport-send ((this rudel-transport) data)
  "Send DATA through THIS transport object.")

(defgeneric rudel-transport-set-handler ((this rudel-transport)
					 handler)
  "Install HANDLER as dispatcher for messages received by THIS.")


;;; Class rudel-transport-backend
;;

(defclass rudel-transport-backend (rudel-backend)
  ()
  "Interface implemented by transport backends."
  :abstract t)

(defgeneric rudel-make-connection ((this rudel-transport-backend) info)
  "Create a transport object according to INFO.")

(defgeneric rudel-wait-for-connections ((this rudel-transport-backend)
					info)
  "Create a transport object according to INFO.")

(provide 'rudel-transport)
;;; rudel-transport.el ends here

;;; rudel-obby.el --- An obby backend for Rudel
;;
;; Copyright (C) 2008, 2009 Jan Moringen
;;
;; Author: Jan Moringen <scymtym@users.sourceforge.net>
;; Keywords: Rudel, obby, backend, implementation
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
;; This file contains a Rudel protocol backend, which implements the
;; obby protocol (used by the Gobby collaborative editor until version
;; 0.5).


;;; History:
;;
;; 0.2 - Refactored client and server to employ state machine.
;;
;; 0.1 - Initial revision.


;;; Code:
;;

(eval-when-compile
  (require 'cl))

(require 'eieio)

(require 'rudel)
(require 'rudel-backend)
(require 'rudel-protocol)
(require 'rudel-util)
(require 'rudel-icons)
(require 'rudel-compat) ;; for `read-color' replacement


;;; Constants
;;

(defconst rudel-obby-version '(0 2)
  "Version of the obby backend for Rudel.")

(defconst rudel-obby-protocol-version 8
  "Version of the obby protocol this library understands.")

(defvar rudel-obby-long-message-threshold 32768
  "Threshold for message size, above which messages are sent in
multiple chunks.")

(defvar rudel-obby-long-message-chunk-size 16384
  "Chunk size used, when chunking long messages.")


;;; Class rudel-obby-backend
;;

(defclass rudel-obby-backend (rudel-protocol-backend)
  ((capabilities :initform '(join host
			     change-color
			     track-subscriptions)))
  "Main class of the Rudel obby backend. Creates obby client
connections and creates obby servers.")

(defmethod initialize-instance ((this rudel-obby-backend) &rest slots)
  "Initialize slots of THIS with SLOTS."
  (when (next-method-p)
    (call-next-method))

  (oset this :version rudel-obby-version))

(defmethod rudel-ask-connect-info ((this rudel-obby-backend) &optional info)
  "Ask user for the information required to connect to an obby server."
  ;; Read server host and port.
  (let ((host            (or (and info (plist-get info :host))
			     (read-string "Server: ")))
	(port            (or (and info (plist-get info :port))
			     (read-number "Port: " 6522)))
	;; Read desired username and color
	(username        (or (and info (plist-get info :username))
			     (read-string "Username: " user-login-name)))
	(color           (or (and info (plist-get info :color))
			     (read-color  "Color: " t)))
	(encryption      (if (and info (member :encryption info))
			     (plist-get info :encryption)
			   (y-or-n-p "Use encryption? ")))
	(global-password (if (and info (member :global-password info))
			     (plist-get info :global-password)
			   (read-string "Global password: " "")))
	(user-password   (if (and info (member :user-password info))
			     (plist-get info :user-password)
			   (read-string "User password: " ""))))
    (append (list :host            host
		  :port            port
		  :username        username
		  :color           color
		  :encryption      encryption
		  :global-password (unless (string= global-password "")
				     global-password)
		  :user-password   (unless (string= user-password "")
				     user-password))
	    info))
  )

(defmethod rudel-connect ((this rudel-obby-backend) info)
  "Connect to an obby server using the information INFO.
Return the connection object."
  ;; Before we start, load the client functionality.
  (require 'rudel-obby-client)

  ;; Create the network process
  (let* ((session    (plist-get info :session))
	 (host       (plist-get info :host))
	 (port       (plist-get info :port))
	 (encryption (plist-get info :encryption))
	 ;; Create the network process
	 (socket     (funcall
		      (if encryption
			  (progn
			    (require 'rudel-tls)
			    #'rudel-tls-make-process)
			#'make-network-process)
		      :name     host
		      :host     host
		      :service  port
		      ;; Install connection filter to redirect data to
		      ;; the connection object
		      :filter   #'rudel-filter-dispatch
		      ;; Install connection sentinel to redirect state
		      ;; changes to the connection object
		      :sentinel #'rudel-sentinel-dispatch
		      ;; Do not start receiving immediately since the
		      ;; filter function is not yet setup properly.
		      :stop     t))
	 (connection (rudel-obby-connection
		      host
		      :session session
		      :socket  socket
		      :info    info)))

    ;; Now start receiving and wait until the basic session setup is
    ;; complete.
    (continue-process socket)

    ;; Wait for the connection to reach one of the states idle,
    ;; join-failed and they-finalized.
    (condition-case error
	(lexical-let ((reporter (make-progress-reporter "Joining ")))
	  (flet ((display-progress (state)
	           (cond
		    ;; For all states, just spin.
		    ((consp state)
		     (progress-reporter-force-update
                      reporter nil (format "Joining (%s)" (car state))))

		    ;; Done
		    (t
		     (progress-reporter-force-update reporter nil "Joining ")
		     (progress-reporter-done reporter)))))

	    (rudel-state-wait connection
			      '(idle) '(join-failed they-finalized)
			      #'display-progress)))

      (rudel-entered-error-state
       (destructuring-bind (symbol . state) (cdr error)
	 (if (eq (rudel-find-state connection 'join-failed) state)
	     (with-slots (error-symbol error-data) state
	       (signal 'rudel-join-error
		       (append (list error-symbol) error-data)))
	   (signal 'rudel-join-error nil)))))

    ;; The connection is now usable; return it.
    connection)
  )

(defmethod rudel-ask-host-info ((this rudel-obby-backend))
  "Ask user for information required to host an obby session."
  (let ((port (read-number "Port: " 6522)))
    (list :port port)))

(defmethod rudel-host ((this rudel-obby-backend) info)
  "Host an obby session using the information INFO.
Return the created server."
  ;; Before we start, we load the server functionality.
  (require 'rudel-obby-server)

  ;; Create the network process.
  (let* ((port   (plist-get info :port))
	 ;; Make a server socket
	 (socket (make-network-process
		  :name     "obby-server"
		  :host     "0.0.0.0"
		  :service  port
		  :server   t
		  :filter   #'rudel-filter-dispatch
		  :sentinel #'rudel-sentinel-dispatch
		  ;;
		  :log
		  (lambda (server-process client-process message)
		    (let ((server (rudel-process-object server-process)))
		      (rudel-add-client server client-process)))))
	 ;; Construct server object.
	 (server (rudel-obby-server "obby-server"
				    :backend this
				    :socket  socket)))

    ;; Return the constructed server.
    server)
  )

(defmethod rudel-make-document ((this rudel-obby-backend)
				name session)
  "Make a new document in SESSION named NAME.
Return the new document."
  ;; Find an unused document id and create a document with that id.
  (let ((id (rudel-available-document-id this session)))
    (with-slots (user-id) (oref session :self)
      (rudel-obby-document name
			   :session  session
			   :id       id
			   :owner-id user-id
			   :suffix   1)))
  )

(defmethod rudel-available-document-id ((this rudel-obby-backend)
					session)
  "Return a document id, which is not in use in SESSION."
  ;; Look through some candidates until an unused id is hit.
  (let* ((used-ids (with-slots (documents) session
		     (mapcar 'rudel-id documents)))
	 (test-ids (number-sequence 0 (length used-ids))))
    (car (sort (set-difference test-ids used-ids) '<)))
  )


;;; Class rudel-obby-user
;;

(defclass rudel-obby-user (rudel-user)
  ((client-id  :initarg  :client-id
	       :type     (or null integer) ;; We allow nil instead of making
	       :accessor rudel-client-id   ;; the slot unbound, to be able to
	       :initform nil               ;; search with test `rudel-client-id
	       :documentation              ;; without headaches
	       "Id of the client connection, which the user used to log in.
The value is an integer, if the user is connected, and nil
otherwise.")
   (user-id    :initarg  :user-id
	       :type     integer
	       :accessor rudel-id
	       :documentation
	       "")
   (connected  :initarg  :connected
	       :type     boolean
	       :accessor rudel-connected
	       :documentation
	       "")
   (encryption :initarg  :encryption ;; TODO maybe we should use unbound when the user is not connected
	       :type     boolean
	       :documentation
	       ""))
  "Class rudel-obby-user ")

(defmethod eieio-speedbar-description ((this rudel-obby-user))
  "Provide a speedbar description for THIS."
  (let ((connected  (oref this :connected))
	(encryption (if (slot-boundp this :encryption)
			(oref this :encryption)
		      nil)))
    (format "User %s (%s, %s)" (object-name-string this)
	    (if connected  "Online" "Offline")
	    (if encryption "Encryption" "Plain")))
  )

(defmethod eieio-speedbar-object-buttonname ((this rudel-obby-user))
  "Return a string to use as a speedbar button for THIS."
  (rudel-display-string this))

(defmethod rudel-display-string ((this rudel-obby-user)
				 &optional use-images align)
  "Return a textual representation of THIS for user interface stuff."
  (with-slots (connected color) this
    (let ((encryption  (and (slot-boundp this :encryption)
			    (oref this :encryption)))
	  (name-string (call-next-method)))
      (concat
       ;; Name bit
       (cond
	((numberp align) (format (format "%-%ds" align) name-string))
	((eq align t)    (format "%-12s" name-string))
	(t		name-string))

       ;; Connection status bit
       (apply
	#'propertize
	(if connected "c" "-")
	'help-echo (format (if connected
			       "%s is connected"
			     "%s is not connected")
			   name-string)
	'face      (list :background color)
	(when use-images
	  (list 'display (if connected
			     rudel-icon-connected
			   rudel-icon-disconnected))))

       ;; Encryption bit
       (apply
	#'propertize
	(if encryption "e" "-")
	'help-echo (format (if encryption
			       "%s's connection is encrypted"
			     "%s's connection is not encrypted")
			   name-string)
	'face      (list :background color)
	(when use-images
	  (list 'display (if encryption
			     rudel-icon-encrypted
			   rudel-icon-plaintext)))))))
  )


;;; Class rudel-obby-document
;;

(defclass rudel-obby-document (rudel-document)
  ((id       :initarg  :id
	     :type     integer
	     :accessor rudel-id
	     :documentation
	     "The id of this document.
The id has to be unique only with respect to the other documents
owned by the owner.")
   (owner-id :initarg  :owner-id
	     :type     integer
	     :documentation
	     "")
   (suffix   :initarg  :suffix
	     :type     integer
	     :documentation
	     "A counter used to distinguish identically named
documents."))
  "Objects of the class rudel-obby-document represent shared
documents in obby sessions.")

(defmethod rudel-both-ids ((this rudel-obby-document))
  "Return a list consisting of document and owner id of THIS document."
  (with-slots ((doc-id :id) owner-id) this
    (list owner-id doc-id)))

(defmethod rudel-unique-name ((this rudel-obby-document))
  "Generate a unique name for THIS based on the name and the suffix."
  (with-slots (suffix) this
    (concat (when (next-method-p)
	      (call-next-method))
	    (when (> suffix 1)
	      (format "<%d>" suffix))))
  )

(defmethod eieio-speedbar-description ((this rudel-obby-document))
  "Construct a description for from the name of document object THIS."
  (format "Document %s" (object-name-string this)))

(defmethod eieio-speedbar-object-buttonname ((this rudel-obby-document))
  "Return a string to use as a speedbar button for OBJECT."
  (with-slots (subscribed) this
    (format "%-12s %s" (object-name-string this)
	    (if subscribed "s" "-")))
  )


;;; Obby message functions
;;

(defun rudel-obby-replace-in-string (string replacements)
  "Replace elements of REPLACEMENTS in STRING.
REPLACEMENTS is a list of conses whose car is the pattern and
whose cdr is the replacement for the pattern."
  (let ((result string))
    (dolist (replacement replacements)
      (let ((from (car replacement))
	    (to   (cdr replacement)))
	(setq result (replace-regexp-in-string
		      from to result nil t))))
    result)
  )

(defun rudel-obby-escape-string (string)
  "Replace meta characters in STRING with their escape sequences."
  (rudel-obby-replace-in-string
   string
   '(("\\\\" . "\\b") ("\n" . "\\n") (":" . "\\d")))
  )

(defun rudel-obby-unescape-string (string)
  "Replace escaped versions of obby meta characters in STRING with the actual meta characters."
  (rudel-obby-replace-in-string
   string
   '(("\\\\n" . "\n") ("\\\\d" . ":") ("\\\\b" . "\\")))
  )

(defun rudel-obby-parse-color (color)
  "Parse the obby color string COLOR into an Emacs color."
  (let* ((color-numeric (string-to-number color 16))
	 (color-string  (format "#%04X%04X%04X"
				(lsh (logand #xff0000 color-numeric) -08)
				(lsh (logand #x00ff00 color-numeric) -00)
				(lsh (logand #x0000ff color-numeric)  08))))
    color-string)
  )

(defun rudel-obby-format-color (color)
  "Format the Emacs color COLOR as obby color string."
  (multiple-value-bind (red green blue) (color-values color)
    (format "%02x%02x%02x" (lsh red -8) (lsh green -8) (lsh blue -8))))

(defun rudel-obby-assemble-message (name &rest arguments)
  ""
  (concat (mapconcat
	   (lambda (part)
	     (if (and (not (null part)) (stringp part))
		 (rudel-obby-escape-string part)
	       part))
	   (cons name arguments) ":")
	  "\n")
  )

(defun rudel-obby-parse-message (message)
  "Split MESSAGE at `:' and unescape resulting parts.

The terminating `\n' should be removed from MESSAGE before
calling this function."
  (mapcar #'rudel-obby-unescape-string (split-string message ":")))

(defun rudel-obby-send (socket name arguments)
  "Send an obby message NAME with arguments ARGUMENTS through SOCKET."
  ;; First, assemble the message string.
  (let ((message (apply #'rudel-obby-assemble-message
			name arguments)))
    (if (>= (length message) rudel-obby-long-message-threshold)
	;; For huge messages, chunk the message data and transmit the
	;; chunks
	(let ((total    (/ (length message)
			   rudel-obby-long-message-chunk-size))
	      (current  0)
	      (reporter (make-progress-reporter "Sending data " 0.0 1.0)))
	  (rudel-loop-chunks message chunk rudel-obby-long-message-chunk-size
	    (progress-reporter-update reporter (/ (float current) total))
	    (process-send-string socket chunk)
	    (incf current))
	  (progress-reporter-done reporter))
      ;; Send small messages in one chunk
      (process-send-string socket message)))
  )


;;; Autoloading
;;

(rudel-add-backend (rudel-backend-get-factory 'protocol)
		   'obby 'rudel-obby-backend)

(eval-after-load 'rudel-zeroconf
  '(rudel-zeroconf-register-service "_lobby._tcp" 'obby))

(provide 'rudel-obby)
;;; rudel-obby.el ends here

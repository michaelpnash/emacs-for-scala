;;; rudel-obby-client.el --- Client functions of the Rudel obby backend
;;
;; Copyright (C) 2008, 2009 Jan Moringen
;;
;; Author: Jan Moringen <scymtym@users.sourceforge.net>
;; Keywords: Rudel, obby, backend, client
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
;; This file contains the client part of the obby backend.


;;; History:
;;
;; 0.2 - State machine.
;;
;; 0.1 - Initial revision.


;;; Code:
;;

(require 'eieio)

(require 'jupiter)

(require 'rudel-state-machine)
(require 'rudel-operations)
(require 'rudel-chat)

(require 'rudel-obby-errors)
(require 'rudel-obby-util)
(require 'rudel-obby-state)


;;; Class rudel-obby-client-state-new
;;

(defclass rudel-obby-client-state-new
  (rudel-obby-client-connection-state)
  ()
  "Start state of newly established connections.")

(defmethod rudel-obby/obby_welcome
  ((this rudel-obby-client-state-new) version)
  "Handle obby 'welcome' message."
  ;; Examine announced protocol version.
  (with-parsed-arguments ((version number))
    (message "Received Obby welcome message (version %d)" version))
  ;; Start encryption handshake
  'encryption-negotiate)


;;; Class rudel-obby-client-state-encryption-negotiate
;;

(defclass rudel-obby-client-state-encryption-negotiate
  (rudel-obby-client-connection-state)
  ()
  "Start state of the encryption handshake.")

(defmethod rudel-obby/net6_encryption
  ((this rudel-obby-client-state-encryption-negotiate) value)
  "Handle net6 'encryption' message."
  (rudel-send this "net6_encryption_ok")
  'encryption-start)


;;; Class rudel-obby-client-connection-encryption-start
;;

(defclass rudel-obby-client-state-encryption-start
  (rudel-obby-client-connection-state)
  ()
  "Second state of the encryption handshake.")

(defmethod rudel-obby/net6_encryption_begin
  ((this rudel-obby-client-state-encryption-start))
  "Handle net6 'encryption_begin' message."
  ;; Start TLS encryption for the connection.
  (with-slots (connection) this
    (with-slots (socket) connection
      (when (rudel-process-object socket :supports-tls)
	(rudel-tls-start-tls socket)
	(sit-for 1))))

  ;; The connection is now established
  'joining)

(defmethod rudel-obby/net6_encryption_failed
  ((this rudel-obby-client-state-encryption-start))
  "Handle net6 'encryption_failed' message."
  ;; The connection is now established; without encryption though
  'joining)


;;; Class rudel-obby-client-state-joining
;;

(defclass rudel-obby-client-state-joining
  (rudel-obby-client-connection-state)
  ()
  "First state after the connection has been properly set up.")

(defmethod rudel-enter ((this rudel-obby-client-state-joining))
  "When entering this state, send a login request."
  ;; Send login request with username and color. This can easily fail
  ;; (resulting in response 'net6_login_failed') if the username or
  ;; color is already taken.
  (with-slots (info) (oref this connection)
    (let ((username        (plist-get info :username))
	  (color           (plist-get info :color))
	  (global-password (plist-get info :global-password))
	  (user-password   (plist-get info :user-password)))
      (apply #'rudel-send
	     this
	     "net6_client_login"
	     username (rudel-obby-format-color color)
	     (append (when global-password
		       (list global-password))
		     (when (and global-password user-password)
		       (list user-password))))))
  nil)

(defmethod rudel-obby/obby_sync_init
  ((this rudel-obby-client-state-joining) count)
  "Handle obby 'sync_init' message."
  ;; Switch to 'synching' state, passing the number of synchronization
  ;; items.
  (with-parsed-arguments ((count number))
    (list 'session-synching count)))

(defmethod rudel-obby/net6_login_failed
  ((this rudel-obby-client-state-joining) reason)
  "Handle net6 'login_failed' message."
  (with-parsed-arguments ((reason number))
    (with-slots (connection) this
      (let ((error-data
	     (cond
	      ;; Invalid username
	      ((= reason rudel-obby-error-username-invalid)
	       (cons 'rudel-obby-username-invalid nil))
	      ;; Username in use
	      ((= reason rudel-obby-error-username-in-use)
	       (cons 'rudel-obby-username-in-use nil))
	      ;; Color in use
	      ((= reason rudel-obby-error-color-in-use)
	       (cons 'rudel-obby-color-in-use nil))
	      ;; Wrong global password
	      ((= reason rudel-obby-error-wrong-global-password)
	       (cons 'rudel-obby-wrong-global-password nil))
	      ;; Wrong user password
	      ((= reason rudel-obby-error-wrong-user-password)
	       (cons 'rudel-obby-wrong-user-password nil))
	      ;; Otherwise, signal a generic join error
	      (t (cons 'rudel-join-error nil)))))

	;; Switch to 'join-failed' state, pass the error data.
	(list 'join-failed error-data))))
  )


;;; Class rudel-obby-client-state-join-failed
;;

(defclass rudel-obby-client-state-join-failed
  (rudel-obby-client-connection-state)
  ((error-symbol :initarg :error-symbol
		 :type    symbol
		 :documentation
		 "Error symbol describing the reason for the
login failure.")
   (error-data   :initarg :error-data
		 :type    list
		 :documentation
		 "Additional error data describing the login
failure."))
  "State for failed login attempts.")

(defmethod rudel-enter ((this rudel-obby-client-state-join-failed)
			error)
  "When the state is entered, store the error data passed in ERROR."
  (with-slots (error-symbol error-data) this
    (setq error-symbol (car error)
	  error-data   (cdr error)))
  nil)


;;; Class rudel-obby-client-state idle
;;

(defclass rudel-obby-client-state-idle
  (rudel-obby-client-connection-state
   rudel-obby-document-handler)
  ()
  "Default state of the connection.")

(defmethod rudel-obby/net6_client_join
  ((this rudel-obby-client-state-idle)
   client-id name encryption user-id color)
  "Handle net6 'client_join' message."
  (with-parsed-arguments ((client-id number)
			  (user-id   number)
			  (color     color))
    (with-slots (connection) this
      (with-slots (session) connection
	(let ((user (rudel-find-user session user-id
				     #'eq #'rudel-id)))
	  (if user
	      ;; If we have such a user object, update its state.
	      (with-slots ((client-id1  client-id)
			   (color1      color)
			   connected
			   (encryption1 encryption)) user
		(setq client-id1  client-id
		      color1      color
		      connected   t
		      encryption1 (string= encryption "1"))

		;; Run the change hook of the user object.
		(object-run-hook-with-args user 'change-hook))
	    ;; Otherwise, create a new user object.
	    (let ((user (rudel-obby-user
			 name
			 :client-id  client-id
			 :user-id    user-id
			 :connected  t
			 :encryption (string= encryption "1")
			 :color      color)))
	      (rudel-add-user session user))))))
    (message "Client joined: %s %s" name color))
  nil)

(defmethod rudel-obby/net6_client_part
  ((this rudel-obby-client-state-idle) client-id)
  "Handle net6 'client_part' message."
  ;; Find the user object, associated to the client id. Remove the
  ;; client id and change the user's state to disconnected.
  (with-parsed-arguments ((client-id number))
    (with-slots (connection) this
      (with-slots (session) connection
	(let ((user (rudel-find-user session client-id
				     #'eql #'rudel-client-id)))
	  (if user
	      (with-slots (client-id connected) user
		;; Set slot values.
		(setq client-id nil
		      connected nil)

		;; Run the change hook of the user object.
		(object-run-hook-with-args user 'change-hook))
	    (display-warning
	     '(rudel obby)
	     (format "Cannot find user for client id: %d"
		     client-id)
	     :warning))))))
  nil)

(defmethod rudel-obby/obby_user_colour
  ((this rudel-obby-client-state-idle) user-id color)
  "Handle obby 'user_colour' message."
  (with-parsed-arguments ((user-id number)
			  (color   color))
    ;; Find user object and set color.
    (with-slots (connection) this
      (with-slots (session) connection
	(let ((user (rudel-find-user session user-id
				     #'= #'rudel-id)))
	  (with-slots ((name :object-name) (color1 :color)) user
	    ;; Set color in user object.
	    (setq color1 color)

	    ;; Run the change hook of the user object.
	    (object-run-hook-with-args user 'change-hook)

	    ;; Update overlays.
	    (rudel-overlay-set-face-attributes
	     (rudel-overlay-make-face-symbol 'author name)
	     color1))))))
  nil)

(defmethod rudel-obby/obby_document_create
  ((this rudel-obby-client-state-idle)
   owner-id doc-id name suffix encoding)
  "Handle obby 'document_create' message."
  (with-parsed-arguments ((owner-id number)
			  (doc-id   number)
			  (suffix   number)
			  (encoding coding-system))
    (with-slots (connection) this
      (with-slots (session) connection
	(let ((owner (rudel-find-user session owner-id
				      #'= #'rudel-id)))
	  (rudel-add-document session (rudel-obby-document
				       name
				       :subscribed (list owner)
				       :id         doc-id
				       :owner-id   owner-id
				       :suffix     suffix))))
      (message "New document: %s" name)))
  nil)

(defmethod rudel-obby/obby_document_remove
  ((this rudel-obby-client-state-idle) doc-id)
  "Handle obby 'document_remove' message."
  (with-parsed-arguments ((doc-id document-id))
    (with-slots (connection) this
      (with-slots (session) connection
	(let ((document (rudel-find-document
			 session doc-id
			 #'equal #'rudel-both-ids)))
	  (if document
	      (progn
		(rudel-remove-document session document)
		(with-slots ((name :object-name)) document
		  (message "Document removed: %s" name)))
	    (display-warning
	     '(rudel obby)
	     (format "Document not found: %s" doc-id)
	     :warning))))))
  nil)

(defmethod rudel-obby/obby_document/rename
  ((this rudel-obby-client-state-idle)
   document user new-name new-suffix)
  "Handle obby 'rename' submessage of the 'obby_document' message."
  (with-parsed-arguments ((new-suffix number))
    (with-slots ((name :object-name) suffix) document
      (setq name   new-name
	    suffix new-suffix)))
  nil)

(defmethod rudel-obby/obby_document/subscribe
  ((this rudel-obby-client-state-idle)
   document user-id)
  "Handle 'subscribe' submessage of obby 'document' message."
  (with-parsed-arguments ((user-id number))
    (with-slots (connection) this
      (with-slots (session) connection
	(let ((user (rudel-find-user session user-id
				     #'= #'rudel-id)))
	  (rudel-add-user document user)))))
  nil)

(defmethod rudel-obby/obby_document/unsubscribe
  ((this rudel-obby-client-state-idle)
   document user-id)
  "Handle 'unsubscribe' submessage of obby 'document' message."
  (with-parsed-arguments ((user-id number))
    (with-slots (connection) this
      (with-slots (session) connection
	(let ((user (rudel-find-user session user-id
				     #'= #'rudel-id)))
	  (rudel-remove-user document user)))))
  nil)

(defmethod rudel-obby/obby_document/record
  ((this rudel-obby-client-state-idle)
   document user-id local-revision remote-revision
   action &rest arguments)
  "Handle 'record' submessage of obby 'document' message."
  (with-parsed-arguments ((user-id         number)
			  (local-revision  number)
			  (remote-revision number))
    ;; Locate the user.
    (let ((user (with-slots (connection) this
		  (with-slots (session) connection
		    (rudel-find-user session user-id
				     #'= #'rudel-id)))))
      (if user
	  (condition-case error
	      ;; Try to dispatch
	      (rudel-dispatch
	       this "rudel-obby/obby_document/record/" action
	       (append (list document user local-revision remote-revision)
		       arguments))
	    ;; Warn if we failed to locate or execute the
	    ;; method. Return nil in this case, so we remain in the
	    ;; current state.
	    (rudel-dispatch-error
	     (progn
	       (display-warning
		'(rudel obby)
		(format "%s: no method (%s: %s): `%s:%s'; arguments: %s"
			(object-print this) (car error) (cdr error)
			"rudel-obby/obby_document/record/" action arguments)
		:debug)
		nil)))
	;; If we did not find the user, warn.
	(progn
	  (display-warning
	   '(rudel obby)
	   (format "User not found: %d" user-id)
	   :warning)
	  nil))))
  )

(defmethod rudel-obby/obby_document/record/ins
  ((this rudel-obby-client-state-idle)
   document user local-revision remote-revision
   position data)
  "Handle 'ins' submessage of 'record' submessage of obby 'document' message."
  (with-parsed-arguments ((position number))
    (let ((operation (jupiter-insert
		      (format "insert-%d-%d"
			      remote-revision local-revision)
		      :from position
		      :data data)))
      (with-slots (connection) this
	(rudel-remote-operation connection
				document user
				remote-revision local-revision
				operation))))
  nil)

(defmethod rudel-obby/obby_document/record/del
  ((this rudel-obby-client-state-idle)
   document user local-revision remote-revision
   position length)
  "Handle 'del' submessage of 'record' submessage of obby 'document' message."
  (with-parsed-arguments ((position number)
			  (length   number))
    (let ((operation (jupiter-delete
		      (format "delete-%d-%d"
			      remote-revision local-revision)
		      :from position
		      :to   (+ position length))))
      (with-slots (connection) this
	(rudel-remote-operation connection
				document user
				remote-revision local-revision
				operation))))
  nil)

(defmethod rudel-obby/obby_document/record/split
  ((this rudel-obby-client-state-idle)
   document user local-revision remote-revision
   &rest operations)
  "Handle 'split' submessage of 'record' submessage of obby 'document' message."
  (let ((operation (rudel-message->operation
		    (cons "split" operations)
		    local-revision remote-revision)))
    (with-slots (connection) this
      (rudel-remote-operation connection
			      document user
			      remote-revision local-revision
			      operation)))
  nil)

(defmethod rudel-obby/obby_document/record/noop
  ((this rudel-obby-client-state-idle)
   document user local-revision remote-revision)
  "Handle 'noop' submessage of 'record' submessage of obby 'document' message."
  (let ((operation (jupiter-nop
		    (format "nop-%d-%d"
			    remote-revision local-revision))))
    (with-slots (connection) this
      (rudel-remote-operation connection
			      document user
			      remote-revision local-revision
			      operation)))
  nil)

(defmethod rudel-obby/obby_message ((this rudel-obby-client-state-idle)
				    sender text)
  "Handle obby 'message' message"
  (with-parsed-arguments ((sender number))
    (with-slots (session) (oref this :connection)
      (let ((sender (rudel-find-user session sender #'eq #'rudel-id)))
	(rudel-chat-dispatch-message sender text))))
  nil)


;;; Class rudel-obby-client-state-session-synching
;;

(defclass rudel-obby-client-state-session-synching
  (rudel-obby-client-connection-state)
  ((all-items       :initarg  :all-items
		    :type     (integer 0)
		    :documentation
		    "Total number of synchronization items expected
		    to receive from the server.")
   (remaining-items :initarg  :remaining-items
		    :type     (integer 0)
		    :documentation
		    "Number of synchronization items not yet
		    received from the server.")
   (have-self       :initarg  :have-self
		    :type     boolean
		    :documentation
		    "Flag that remembers, whether the session has
		    a 'self' user object."))
  "State used for synching session data.")

(defmethod rudel-enter ((this rudel-obby-client-state-session-synching)
			num-items)
  "When entering state, store number of expected items."
  (with-slots (all-items remaining-items have-self) this
    (setq all-items       num-items
	  remaining-items num-items
	  have-self       nil))
  nil)

(defmethod rudel-obby/net6_client_join
  ((this rudel-obby-client-state-session-synching)
   client-id name encryption user-id color)
  "Handle net6 'client_join' message."
  (with-parsed-arguments ((client-id number)
			  (user-id   number)
			  (color     color))
    (with-slots (connection remaining-items have-self) this
      (with-slots (session) connection
	;; Construct user object and add it to the session.
	(let ((user (rudel-obby-user
		     name
		     :client-id  client-id
		     :user-id    user-id
		     :connected  t
		     :encryption (string= encryption "1")
		     :color      color)))
	  (rudel-add-user session user)

	  ;; The first user object describes the user of this client.
	  (unless have-self
	    (with-slots (self) session
	      (setq self      user
		    have-self t)))))

      ;; Decrease number of not yet received synchronization items.
      (decf remaining-items)))
  nil)

(defmethod rudel-obby/obby_sync_usertable_user
  ((this rudel-obby-client-state-session-synching) user-id name color)
  "Handle obby 'sync_usertable_user' message."
  (with-parsed-arguments ((user-id number)
			  (color   color))
    (with-slots (connection remaining-items) this
      (with-slots (session) connection
	(rudel-add-user session (rudel-obby-user
				 name
				 :user-id    user-id
				 :connected  nil
				 :color      color)))

      ;; Decrease number of not yet received synchronization items.
      (decf remaining-items)))
  nil)

(defmethod rudel-obby/obby_sync_doclist_document
  ((this rudel-obby-client-state-session-synching)
   owner-id doc-id name suffix encoding &rest subscribed-user-ids)
  "Handle obby 'sync_doclist_document' message."
  (with-parsed-arguments ((doc-id   number)
			  (owner-id number)
			  (suffix   number)
			  (encoding coding-system))
    (with-slots (connection remaining-items) this
      (with-slots (session) connection
	;; Retrieve the subscribed users
	(let ((subscribed-users
	       (mapcar
		(lambda (user-id)
		  (with-parsed-arguments ((user-id number))
		    (rudel-find-user session user-id
				     #'= #'rudel-id)))
		subscribed-user-ids)))

	  ;; Make a new document with the list of subscribed users.
	  (rudel-add-document session (rudel-obby-document
				       name
				       :subscribed subscribed-users
				       :id         doc-id
				       :owner-id   owner-id
				       :suffix     suffix))))

      ;; Decrease number of not yet received synchronization items.
      (decf remaining-items)))
  nil)

(defmethod rudel-obby/obby_sync_final
  ((this rudel-obby-client-state-session-synching))
  "Handle obby 'sync_final' message."
  'idle)

(defmethod object-print ((this rudel-obby-client-state-session-synching)
			 &rest strings)
  "Append number of remaining items to string representation."
  (with-slots (remaining-items) this
    (call-next-method this (format " remaining: %d" remaining-items))))


;;; Class rudel-obby-client-state-subscribing
;;

(defclass rudel-obby-client-state-subscribing
  (rudel-obby-client-connection-state
   rudel-obby-document-handler)
  ((document :initarg :document
	     :type    rudel-obby-document-child
	     :documentation
	     ""))
  "")

(defmethod rudel-enter ((this rudel-obby-client-state-subscribing)
			user document)
  "When entering this state, send a subscription request to the server."
  (with-slots ((document1 :document)) this
    (setq document1 document)

    (with-slots ((doc-id :id) owner-id) document1
      (with-slots (user-id) user
	(rudel-send this "obby_document"
		    (format "%x %x" owner-id doc-id)
		    "subscribe"
		    (format "%x" user-id)))))
  nil)

(defmethod rudel-obby/obby_document/sync_init
  ((this rudel-obby-client-state-subscribing)
   document num-bytes)
  "Handle obby 'sync_init' message."
  (with-parsed-arguments ((num-bytes number))
    (with-slots (documents) this
      (if (= num-bytes 0)
	  'idle
	(list 'document-synching document num-bytes))))
  )


;;; Class rudel-obby-client-state-document-synching
;;

(defclass rudel-obby-client-state-document-synching
  (rudel-obby-client-connection-state
   rudel-obby-document-handler)
  ((document        :initarg  :document
		    :type     rudel-obby-document-child
		    :documentation
		    "")
   (all-bytes       :initarg  :all-bytes
		    :type     (integer 0)
		    :documentation
		    "")
   (remaining-bytes :initarg  :remaining-bytes
		    :type     (integer 0)
		    :documentation
		    ""))
  "")

(defmethod rudel-enter ((this rudel-obby-client-state-document-synching)
			document num-bytes)
  ""
  (with-slots ((document1 :document) all-bytes remaining-bytes) this
    (setq document1       document
	  all-bytes       num-bytes
	  remaining-bytes num-bytes))
  nil)

(defmethod rudel-obby/obby_document/sync_chunk
  ((this rudel-obby-client-state-document-synching)
   document data user-id)
  "Handle obby 'sync_chunk' message."
  (with-parsed-arguments ((user-id number))
    (with-slots (connection remaining-bytes) this
      (with-slots (session) connection
	(let* ((user      (unless (zerop user-id)
			    (rudel-find-user session user-id
					     #'= #'rudel-id)))
	       (operation (rudel-insert-op "bulk-insert"
					   :from nil
					   :data data)))
	  (rudel-remote-operation document user operation)))

      ;; After all bytes are transferred, go back to idle state.
      (decf remaining-bytes (string-bytes data))
      (if (= remaining-bytes 0)
	  'idle
	nil)))
  )

(defmethod object-print ((this rudel-obby-client-state-document-synching)
			 &rest strings)
  "Append number of remaining items to string representation."
  (with-slots (remaining-bytes) this
    (call-next-method this (format " remaining: %d" remaining-bytes))))


;;; Class rudel-obby-client-state-they-finalized
;;

(defclass rudel-obby-client-state-they-finalized
  (rudel-obby-client-connection-state)
  ()
  "State used to indicate that the connection was closed by the peer.")


;;; Client connection states.
;;

(defvar rudel-obby-client-connection-states
  '((new                  . rudel-obby-client-state-new)
    (encryption-negotiate . rudel-obby-client-state-encryption-negotiate)
    (encryption-start     . rudel-obby-client-state-encryption-start)
    (joining              . rudel-obby-client-state-joining)
    (join-failed          . rudel-obby-client-state-join-failed)
    (idle                 . rudel-obby-client-state-idle)
    (session-synching     . rudel-obby-client-state-session-synching)
    (subscribing          . rudel-obby-client-state-subscribing)
    (document-synching    . rudel-obby-client-state-document-synching)
    (they-finalized       . rudel-obby-client-state-they-finalized))
  "Name symbols and classes of connection states.")


;;; Class rudel-obby-connection
;;

(defclass rudel-obby-connection (rudel-obby-socket-owner
				 rudel-connection
				 rudel-state-machine)
  ((info     :initarg :info
	     :type    list
	     :documentation
	     "Stores connection information for later use.")
   (contexts :initarg :contexts
	     :type    hash-table
	     :documentation
	     "Contains jupiter context objects for all
documents."))
  "Class rudel-obby-connection ")

(defmethod initialize-instance ((this rudel-obby-connection) &rest slots)
  ;; Initialize slots of THIS
  (when (next-method-p)
    (call-next-method))

  ;; Create a new hash-table object to hold jupiter contexts
  ;; associated to documents.
  (with-slots (contexts) this
    (setq contexts (make-hash-table :test #'equal)))

  ;; Register states.
  (rudel-register-states this rudel-obby-client-connection-states)
  )

(defmethod rudel-register-state ((this rudel-obby-connection)
				 symbol state)
  "Register SYMBOL and STATE and set connection slot of STATE."
  ;; Associate THIS connection to STATE.
  (oset state :connection this)

  ;; Register STATE.
  (when (next-method-p)
    (call-next-method))
  )

(defmethod rudel-disconnect ((this rudel-obby-connection))
  ""
  (when (next-method-p)
    (call-next-method)))

(defmethod rudel-close ((this rudel-obby-connection))
  ""
  ;; Move the state machine into an error state.
  (rudel-switch this 'they-finalized)

  ;; Terminate the session.
  (with-slots (session) this
    (rudel-end session)))

(defmethod rudel-find-context ((this rudel-obby-connection) document)
  "Return the jupiter context associated to DOCUMENT in THIS connection."
  (with-slots (contexts) this
    (gethash (oref document :id) contexts)))

(defmethod rudel-add-context ((this rudel-obby-connection) document)
  "Add a jupiter context for DOCUMENT to THIS connection."
  (with-slots (contexts) this
    (with-slots ((doc-name :object-name) (doc-id :id)) document
      (puthash doc-id
	       (jupiter-context (format "%s" doc-name))
	       contexts)))
  )

(defmethod rudel-remove-context ((this rudel-obby-connection) document)
  "Remove the jupiter context associated to DOCUMENT from THIS connection."
  (with-slots (contexts) this
    (remhash (oref document :id) contexts)))

(defmethod rudel-message ((this rudel-obby-connection) message)
  "Dispatch MESSAGE to the current state of THIS object.
If the state has no suitable method, generate a warning, but do
nothing else."
  ;; Dispatch message to state.
  (rudel-accept this message))

(defmethod rudel-change-color- ((this rudel-obby-connection) color)
  ""
  (rudel-send this "obby_user_colour"
	      (rudel-obby-format-color color)))

(defmethod rudel-publish ((this rudel-obby-connection) document)
  ""
  ;; Create a new jupiter context for DOCUMENT.
  (rudel-add-context this document)

  ;; Announce the new document to the server.
  (with-slots ((name :object-name) id buffer) document
    (rudel-send this "obby_document_create"
		(format "%x" id)
		name
		"UTF-8"
		(with-current-buffer buffer
		  (buffer-string))))
  )

(defmethod rudel-unpublish ((this rudel-obby-connection) document)
  "Remove DOCUMENT from the obby session THIS is connected to."
  ;; Request removal of DOCUMENT.
  (with-slots ((doc-id :id) owner-id) document
      (rudel-send this "obby_document_remove"
		  (format "%x %x" owner-id doc-id)))

  ;; Remove the jupiter context for DOCUMENT.
  (rudel-remove-context this document)
  )

(defmethod rudel-subscribe-to ((this rudel-obby-connection) document)
  ""
  ;; Create a new jupiter context for DOCUMENT.
  (rudel-add-context this document)

  ;; Switch to subscribing state and wait until the state goes back to
  ;; idle.
  (with-slots (session) this
    (with-slots (self) session
      (rudel-switch this 'subscribing self document)))

  (lexical-let ((reporter (make-progress-reporter "Subscribing " 0.0 1.0)))
    (flet ((display-progress (state)
	     (cond
	      ;; Syncing document content, we can provide detailed progress.
	      ((and (consp state)
		    (eq (car state) 'document-synching))
	       (with-slots (all-bytes remaining-bytes) (cdr state)
		 (progress-reporter-force-update
		  reporter
		  (- 1.0 (/ (float remaining-bytes) (float all-bytes)))
		  (format "Subscribing (%s) " (car state)))))

	      ;; For other states, we just spin.
	      ((consp state)
	       (progress-reporter-force-update
	        reporter 0.5
	        (format "Subscribing (%s) " (car state))))

	      ;; Done
	      (t
	       (progress-reporter-force-update reporter 1.0 "Subscribing ")
	       (progress-reporter-done reporter)))))
      (rudel-state-wait this '(idle) '(they-finalized) #'display-progress)))

  ;; We receive a notification of our own subscription from the
  ;; server. Consequently we do not add SELF to the list of subscribed
  ;; users of DOCUMENT.
  )

(defmethod rudel-unsubscribe-from ((this rudel-obby-connection) document)
  ""
  ;; Delete the jupiter context for DOCUMENT.
  (rudel-remove-context this document)

  ;; Announce the end of our subscription to the server.
  (with-slots (session) this
    (with-slots (user-id) (oref session :self)
      (with-slots ((doc-id :id) owner-id) document
	(rudel-send this "obby_document"
		    (format "%x %x" owner-id doc-id)
		    "unsubscribe"
		    (format "%x" user-id)))))

  ;; We receive a notification of the end of our own subscription from
  ;; the server. Consequently we do not remove SELF from the list of
  ;; subscribed users of DOCUMENT.
  )

(defmethod rudel-local-insert ((this rudel-obby-connection)
			       document position data)
  ""
  (rudel-local-operation
   this
   document
   (jupiter-insert "insert" :from position :data data)))

(defmethod rudel-local-delete ((this rudel-obby-connection)
			       document position length)
  ""
  (rudel-local-operation
   this
   document
   (jupiter-delete "delete" :from position :to (+ position length))))

(defmethod rudel-local-operation ((this rudel-obby-connection)
				  document operation)
  "Handle OPERATION performed on DOCUMENT by sending a message through THIS connection."
  ;; Convert character positions in OPERATION to byte positions, since
  ;; the obby protocol works with byte positions, but Emacs uses
  ;; character positions.
  (with-slots (buffer) document
    (rudel-obby-char->byte operation buffer))

  ;; Find jupiter context for DOCUMENT.
  (let ((context (rudel-find-context this document)))

    ;; Notify the server of the operation.
    (with-slots (owner-id (doc-id :id)) document
      (with-slots (local-revision remote-revision) context
	(apply #'rudel-send
	       this
	       "obby_document"
	       (format "%x %x" owner-id doc-id)
	       "record"
	       (format "%x" local-revision)
	       (format "%x" remote-revision)
	       (rudel-operation->message operation))))

      ;; Submit the operation to the jupiter context.
      (jupiter-local-operation context operation))
  )

(defmethod rudel-remote-operation ((this rudel-obby-connection)
				   document user
				   remote-revision local-revision
				   operation)
  "Handle OPERATION received through THIS connection performed by USER on DOCUMENT."
  (let* (;; Find jupiter context for DOCUMENT.
	 (context     (rudel-find-context this document))
	 ;; And transform the operation.
	 (transformed (jupiter-remote-operation
		       context
		       remote-revision local-revision
		       operation)))

    ;; Convert byte positions in OPERATION to character positions,
    ;; since the obby protocol works with byte positions, but Emacs
    ;; uses character positions.
    (with-slots (buffer) document
      (rudel-obby-byte->char transformed buffer))  ;; TODO operation's responsibility?

    ;; Apply the transformed operation to the document.
    (rudel-remote-operation document user transformed))
  )

(provide 'rudel-obby-client)
;;; rudel-obby-client.el ends here

;;; rudel.el --- A collaborative editing framework for Emacs
;;
;; Copyright (C) 2008, 2009 Jan Moringen
;;
;; Author: Jan Moringen <scymtym@users.sourceforge.net>
;; Keywords: rudel, collaboration
;; URL: http://rudel.sourceforge.net/
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
;; along with rudel. If not, see <http://www.gnu.org/licenses>.


;;; Commentary:
;;
;; Rudel is a framework for collaborative editing in Emacs.  Its
;; architecture allows communication with arbitrary collaborative
;; editors.


;;; History:
;;
;; 0.1 - Initial revision.


;;; Code:
;;

(eval-when-compile
  (require 'cl))

(require 'eieio)
(require 'eieio-base)

(require 'eieio-speedbar) ;; TODO required for now

(require 'rudel-util)
(require 'rudel-backend)
(require 'rudel-session-initiation)
(require 'rudel-operations)
(require 'rudel-operators)
(require 'rudel-overlay)
(require 'rudel-hooks)
(require 'rudel-interactive) ;; for `rudel-read-backend',
			     ;; `rudel-read-document',
			     ;; `rudel-read-session'
(require 'rudel-icons)
(require 'rudel-compat) ;; for `read-color' replacement


;;; Global variables
;;

(defconst rudel-version '(0 2)
  "Version of the Rudel framework.")

(defvar rudel-current-session nil
  "Global object representing the current Rudel session.
nil if there is no active session.")

(defvar rudel-buffer-document nil
  "Buffer-local variable which holds the Rudel document associated with the buffer.")
(make-variable-buffer-local 'rudel-buffer-document)
(put 'rudel-buffer-document 'permanent-local t)

(defvar rudel-buffer-change-workaround-data nil
  "Buffer-local variable which holds change data that could not be accessed otherwise.
It would be nice to find another way to do this.")
(make-variable-buffer-local 'rudel-buffer-change-workaround-data)
(put 'rudel-buffer-change-workaround-data 'permanent-local t)


;;; Customization
;;

(defgroup rudel nil
  "Rudel collaborative editing framework."
  :group 'applications)

(defcustom rudel-allocate-buffer-function
  'rudel-allocate-buffer-clear-existing
  "A function used to find or create buffers to associate to documents.
The function is called with the document name as the sole
argument and has to return a buffer object which will be attached
to the document in question."
  :group   'rudel
  :type    '(choice (const :tag "Clear content of existing buffer"
			   rudel-allocate-buffer-clear-existing )
		    (const :tag "Create a new uniquely named buffer"
			   rudel-allocate-buffer-make-unique )
		    (function :tag "Other function"))
  :require 'rudel-interactive)

(defcustom rudel-default-username (user-login-name)
  "*"
  :group 'rudel
  :type  '(string))


;;; Class rudel-session
;;

(defclass rudel-session (rudel-hook-object)
  ((backend              :initarg  :backend
			 :type     rudel-backend-child
			 :documentation
			 "The backend used by this session.")
   (users                :initarg  :users
			 :type     list
			 :initform nil
			 :documentation
			 "The list of users participating in this
session.")
   (documents            :initarg  :documents
			 :type     list
			 :initform nil
			 :documentation
			 "This list of documents available in
this session.")
   ;; Hooks
   (end-hook             :initarg  :end-hook
			 :type     list
			 :initform nil
			 :documentation
			 "")
   (add-user-hook        :initarg  :add-user-hook
			 :type     list
			 :initform nil
			 :documentation
			 "This hook is run when a user gets added
to the session.
The arguments are the session and the user object.")
   (remove-user-hook     :initarg  :remove-user-hook
			 :type     list
			 :initform nil
			 :documentation
			 "This hook is run when a user gets
removed from the session.
The arguments are the session and the user object.")
   (add-document-hook    :initarg  :add-document-hook
			 :type     list
			 :initform nil
			 :documentation
			 "This hook is run when a document gets
added to the session.
The arguments are the session and the document object.")
   (remove-document-hook :initarg  :remove-document-hook
			 :type     list
			 :initform nil
			 :documentation
			 "This hook is run when a document gets
removed from the session.
The arguments are the session and the document object."))
  "This class serves as a base class for rudel-client-session and
rudel-server-session. Consequently, it consists of slots common
to client and server sessions."
  :abstract t)

(defmethod rudel-end ((this rudel-session))
  "Terminate THIS session performing all necessary cleanup."
  ;; Run the hook.
  (object-run-hook-with-args this 'end-hook))

(defmethod rudel-add-user ((this rudel-session) user)
  "Add USER to the user list of THIS session.

Runs object hook (see `rudel-hook-object') `add-user-hook' with
arguments THIS and USER."
  ;; Add USER to list.
  (object-add-to-list this :users user)

  ;; Run the hook.
  (object-run-hook-with-args this 'add-user-hook user))

(defmethod rudel-remove-user ((this rudel-session) user)
  "Remove USER from the user list of THIS session.

Runs object hook (see `rudel-hook-object') `remove-user-hook'
with arguments THIS and USER."
  ;; Remove USER from list.
  (object-remove-from-list this :users user)

  ;; Run the hook.
  (object-run-hook-with-args this 'remove-user-hook user))

(defmethod rudel-find-user ((this rudel-session)
			    which &optional test key)
  "Find user WHICH in the user list.
WHICH is compared to the result of KEY using TEST."
  (unless test
    (setq test #'string=))
  (unless key
    (setq key #'object-name-string))
  (with-slots (users) this
    (find which users :key key :test test))
  )

(defmethod rudel-add-document ((this rudel-session) document)
  ""
  (unless (slot-boundp document :session)
    (oset document :session this))

  ;; Add DOCUMENT to the list of documents in THIS session.
  (object-add-to-list this :documents document)

  ;; Run the hook.
  (object-run-hook-with-args this 'add-document-hook document))

(defmethod rudel-remove-document ((this rudel-session) document)
  "Remove DOCUMENT from THIS session, detaching it if necessary."
  ;; Detach document from its buffer when necessary.
  (when (rudel-attached-p document)
    (rudel-detach-from-buffer document))

  ;; Remove DOCUMENT from the list of documents in THIS session.
  (object-remove-from-list this :documents document)

  ;; Run the hook.
  (object-run-hook-with-args this 'remove-document-hook document))

(defmethod rudel-find-document ((this rudel-session)
				which &optional test key)
  "Find document WHICH in the document list.
WHICH is compared to the result of KEY using TEST."
  (unless test
    (setq test #'string=))
  (unless key
    (setq key #'object-name-string))
  (with-slots (documents) this
    (find which documents :key key :test test))
  )

(defmethod rudel-unsubscribed-documents ((this rudel-session))
  ""
  (unless (slot-boundp this :self)
    (error "Cannot find unsubscribed documents unless slot self is bound"))
  (with-slots (documents self) this
    (remove-if
     (lambda (document)
       (with-slots (subscribed) document
	 (memq self subscribed)))
       documents))
  )


;;; Class rudel-client-session
;;
(defclass rudel-client-session (rudel-session)
  ((connection :initarg  :connection
	       :type     (or null rudel-connection-child)
	       :initform nil
	       :documentation
	       "The connection used for communication by this
session.")
   (self       :initarg  :self
	       :type     rudel-user-child
	       :documentation
	       "Points into USERS to the user object representing
the local user"))
  "Objects represent a collaborative editing session from a
client perspective.")

(defmethod rudel-end ((this rudel-client-session))
  ;; Clean everything up
  (with-slots (connection users documents) this
    ;; Detach all documents from their buffers
    (mapc #'rudel-detach-from-buffer documents)

    ;; Terminate the connection
    (when connection
      (condition-case nil
	  (rudel-disconnect connection)
	(error nil))))

  ;;
  (when (next-method-p)
    (call-next-method))
  )


;;; Class rudel-server-session
;;

(defclass rudel-server-session (rudel-session)
  ()
  "Class rudel-server-session "
  :abstract t)


;;; Class rudel-connection
;;

(defclass rudel-connection ()
  ((session :initarg :session
	    :type    rudel-session-child
	    :documentation
	    ""))
  "This abstract class defines the interface implementations of
client protocols have to obey."
  :abstract t)

(defgeneric rudel-disconnect ((this rudel-connection))
  "Close the connection.")

(defgeneric rudel-change-color- ((this rudel-connection) color) ;; TODO name
  "")

(defgeneric rudel-publish ((this rudel-connection) document)
  "")

(defgeneric rudel-subscribe-to ((this rudel-connection) document)
  "")

(defgeneric rudel-unsubscribe-from ((this rudel-connection) document) ; TODO name should be rudel-unsubscribe
  "")

(defgeneric rudel-local-insert ((this rudel-connection))
  "")

(defgeneric rudel-local-delete ((this rudel-connection))
  "")

(defgeneric rudel-remote-insert ((this rudel-connection))
  "")

(defgeneric rudel-remote-delete ((this rudel-connection))
  "")


;;; Class rudel-user
;;

(defclass rudel-user (eieio-named
		      eieio-speedbar-file-button
		      rudel-hook-object)
  ((color       :initarg  :color
		:accessor rudel-color
		:documentation
		"Color used to indicate ownership or authorship
by the user. Examples includes text written by the user or the
user name itself.")
   (change-hook :initarg  :change-hook
		:type     list
		:initform nil
		:documentation
		"This hook is run when this user object
changes."))
  "Objects of this class represent users participating in
collaborative editing session. Note that a participating user
does not have to be connected to the session at any given time."
  :abstract t)

(defmethod rudel-display-string ((this rudel-user)
				 &optional use-images align)
  "Return a textual representation of THIS for user interface stuff."
  (with-slots ((name :object-name) color) this
    (propertize
     (concat
      (when use-images
	(propertize "*" 'display rudel-icon-person))
      name)
     'face (list :background color)))
  )


;;; Class rudel-document
;;

(defclass rudel-document (eieio-named
			  eieio-speedbar-file-button
			  rudel-hook-object)
  ((session          :initarg  :session
		     :type     rudel-session
		     :documentation
		     "")
   (buffer           :initarg  :buffer
		     :type     (or null buffer)
		     :initform nil
		     :documentation
		     "")
   (subscribed       :initarg  :subscribed
		     :type     list
		     :initform nil
		     :documentation
		     "")
   ;; Hooks
   (subscribe-hook   :initarg  :subscribe-hook
		     :type     list
		     :initform nil
		     :documentation
		     "This hook is run when a user subscribes to
this document object.")
   (unsubscribe-hook :initarg  :unsubscribe-hook
		     :type     list
		     :initform nil
		     :documentation
		     "This hook is run when a user unsubscribes
from this document object.")
   (attach-hook      :initarg  :attach-hook
		     :type     list
		     :initform nil
		     :documentation
		     "This hook is run when a buffer is attached
to this document object.")
   (detach-hook      :initarg  :detach-hook
		     :type     list
		     :initform nil
		     :documentation
		     "This hook is run when the attached buffer
is detached from this document object."))
  "This class represents a document, which participants of a
collaborative editing session can subscribe to."
  :abstract t)

(defmethod rudel-unique-name ((this rudel-document))
  "Returns a suggested name for the buffer attached to THIS document."
  (object-name-string this))

(defmethod rudel-suggested-buffer-name ((this rudel-document))
  "Returns a suggested name for the buffer attached to THIS document."
  (rudel-unique-name this))

(defmethod rudel-attached-p ((this rudel-document))
  (with-slots (buffer) this
    buffer))

(defmethod rudel-attach-to-buffer ((this rudel-document) buffer)
  "Attach THIS document to BUFFER"
  (with-slots ((doc-buffer :buffer)) this
    ;; Set buffer slot of THIS to BUFFER and associated THIS with
    ;; BUFFER.
    (setq doc-buffer buffer)
    (rudel-set-buffer-document this buffer)

    (with-current-buffer doc-buffer
      ;; Add the handler function for buffer changes to the buffer's
      ;; change hook.
      (add-hook 'after-change-functions
		#'rudel-handle-buffer-change
		nil t)

      ;; Store change data before the change a done. This is necessary
      ;; because the number of bytes (not characters) cannot otherwise
      ;; be recovered after a deletion.
      (add-hook 'before-change-functions
		#'rudel-buffer-change-workaround
		nil t)

      ;; Add a handler to the kill-buffer hook to unsubscribe from the
      ;; document when the buffer gets killed.
      (add-hook 'kill-buffer-hook
		#'rudel-unpublish-buffer
		nil t)

      ;;
      (add-hook 'change-major-mode-hook
		#'rudel-handle-major-mode-change
		nil t))

    ;; Run the hook.
    (object-run-hook-with-args this 'attach-hook doc-buffer))
  )

(defmethod rudel-detach-from-buffer ((this rudel-document))
  "Detach document THIS from its buffer.
Do nothing, if THIS is not attached to any buffer."
  (with-slots (buffer) this
    (let ((buffer-save buffer))

      ;; Only try to detach from BUFFER, if it is non-nil. BUFFER can
      ;; be nil, if the user did not subscribe to the document, or
      ;; unsubscribed after subscribing.
      (when buffer

	(with-current-buffer buffer
	  ;; Remove our handler function from the kill-buffer hook.
	  (remove-hook 'kill-buffer-hook
		       #'rudel-unpublish-buffer
		       t)

	  ;; Remove our handler function from the after-change hook.
	  (remove-hook 'after-change-functions
		       #'rudel-handle-buffer-change
		       t)

	  ;; Remove our handler function from the before-change hook.
	  (remove-hook 'before-change-functions
		       #'rudel-buffer-change-workaround
		       t)

	  ;; Remove all overlays.
	  (rudel-overlays-remove-all)

	  ;; Remove the major mode change handler.
	  (remove-hook 'change-major-mode-hook
		       #'rudel-handle-major-mode-change
		       t))

	;; Unset buffer slot of THIS and delete association of THIS with
	;; BUFFER.
	(rudel-set-buffer-document nil buffer)
	(setq buffer nil))

      ;; Run the hook.
      (object-run-hook-with-args this 'detach-hook buffer-save)))
  )

(defmethod rudel-add-user ((this rudel-document) user)
  "Add USER to the list of subscribed users of THIS.

Runs object hook (see `rudel-hook-object') `subscribe-hook' with
arguments THIS and USER."
  ;; Add USER to list.
  (object-add-to-list this :subscribed user)

  ;; Run the hook.
  (object-run-hook-with-args this 'subscribe-hook user))

(defmethod rudel-remove-user ((this rudel-document) user)
  "Remove USER from the list of subscribed users of THIS.

Runs object hook (see `rudel-hook-object') `unsubscribe-hook'
with arguments THIS and USER."
  ;; Remove USER from list.
  (object-remove-from-list document :subscribed user)

  ;; Run the hook.
  (object-run-hook-with-args this 'unsubscribe-hook user))

(defmethod rudel-insert ((this rudel-document) position data)
  "Insert DATA at POSITION into the buffer attached to THIS.
When POSITION is nil `point-max' is used to determine the
insertion position.
Modification hooks are disabled during the insertion."
  (with-slots (buffer) this
    (save-excursion
      (set-buffer buffer)

      (unless position
	(setq position (- (point-max) 1)))

      (let ((inhibit-modification-hooks t))
	(goto-char (+ position 1))
	(insert data))))
  )

(defmethod rudel-delete ((this rudel-document) position length)
  "Delete a region of LENGTH character at POSITION from the buffer attached to THIS.
Modification hooks are disabled during the insertion."
  (with-slots (buffer) this
    (save-excursion
      (set-buffer buffer)

      (let ((inhibit-modification-hooks t))
	(delete-region (+ position 1) (+ position length 1)))))
  )

(defmethod rudel-local-operation ((this rudel-document) operation)
  "Apply the local operation OPERATION to THIS."
  (with-slots (session buffer) this
    (with-slots (connection (user :self)) session
      (dolist (operators (list

			   ;; Update overlays
			   (rudel-overlay-operators
			    "overlay-operators"
			    :document this
			    :user     user)

			   ;; Notify connection
			   (rudel-connection-operators
			    "connection-operators"
			    :connection connection
			    :document   this)))

	;; Apply the operation using each set of operators
	(rudel-apply operation operators))))
  )

(defmethod rudel-remote-operation ((this rudel-document) user operation)
  "Apply the remote operation OPERATION performed by USER to THIS."
  (dolist (operators (append

		       ;; Update buffer contents
		       (list (rudel-document-operators
			      "document-operators"
			      :document this))

		       ;; Update overlays
		       (when user
			 (list (rudel-overlay-operators
				"overlay-operators"
				:document this
				:user     user)))))

    ;; Apply the operation using each set of operators
    (rudel-apply operation operators))
  )

(defmethod rudel-chunks ((this rudel-document))
  "Return a list of text chunks of the associated buffer.
Each element in the chunk is a list structured like this (START
END AUTHOR). START and END are numbers, AUTHOR is of type (or
null rudel-user-child)."
  (with-slots (buffer) this
    ;; Extract buffer string and a list of chunks partitioning the
    ;; string according to the respective author (or nil).
    (with-current-buffer buffer
      (let ((string         (buffer-string)) ;; TODO no-properties?
	    (overlay-chunks (mapcar
			     (lambda (overlay)
			       (list (- (overlay-start overlay) 1)
				     (- (overlay-end   overlay) 1)
				     (rudel-overlay-user overlay)))
			     (sort* (rudel-author-overlays)
				    '< :key 'overlay-start)))
	    (last)
	    (augmented-chunks))

	;; Iterate through the list of chunks to find gaps between
	;; chunks (also before the first) and insert entries with
	;; author nil accordingly.
	(dolist (chunk overlay-chunks)
	  (when (or (and (not last)
			 (> (nth 0 chunk) 0))
		    (and last
			 (/= (nth 1 last)
			     (nth 0 chunk))))
	    (push (list (if last (nth 1 last) 0)
			(nth 0 chunk)
			nil)
		  augmented-chunks))
	  (push chunk augmented-chunks)
	  (setq last chunk))

	;; If there is text after the last chunk, create another one
	;; with author nil. If there were no chunks at all, this chunk
	;; can also cover the whole buffer string.
	(when (or (and (not last)
		       (/= (length string) 0))
		  (and last
		       (/= (nth 1 last) (length string))))
	  (push (list (if last (nth 1 last) 0)
		      (length string)
		      nil)
		augmented-chunks))

	;; Sort chunks according to the start position.
	(sort* augmented-chunks '< :key 'car))))
  )


;;; Buffer-related functions
;;

(defun rudel-buffer-has-document-p (&optional buffer)
  "Return non-nil if a document object is attached to BUFFER.
If BUFFER is nil, use the current buffer."
  (unless buffer
    (setq buffer (current-buffer)))

  (buffer-local-value 'rudel-buffer-document buffer))

(defun rudel-buffer-document (&optional buffer)
  "Return the document object attached to BUFFER.
If BUFFER is nil, use the current buffer."
  (unless buffer
    (setq buffer (current-buffer)))

  (buffer-local-value 'rudel-buffer-document buffer))

(defun rudel-set-buffer-document (document &optional buffer)
  "Associate BUFFER to DOCUMENT.
If DOCUMENT is nil, make it not associated to any buffer.
If BUFFER is nil, use the current buffer."
  (unless buffer
    (setq buffer (current-buffer)))

  (with-current-buffer buffer
    (setq rudel-buffer-document document)))

(defun rudel-handle-buffer-change (from to length)
  "Handle buffer change at range FROM to TO with length LENGTH by relaying them to the document object of the buffer.
See after-change-functions for more information."
  (when (rudel-buffer-has-document-p)
    (let ((document (rudel-buffer-document))
	  (text)) ; TODO with-rudel-buffer-document?
      (cond
       ;; The change was an insert
       ((and (/= from to)
	     (zerop length))
	(with-slots (buffer) document
	  (with-current-buffer buffer
	    (setq text (buffer-substring-no-properties from to)))
	  (rudel-local-operation document
				 (rudel-insert-op
				  "insert"
				  :from (- from 1)
				  :data text))))

       ;; The change was a delete
       ((and (= from to)
	     (not (zerop length)))
	(rudel-local-operation document
			       (rudel-delete-op
				"delete"
				:from   (- from 1)
				:length length)))

       ;; The operation was neither an insert nor a delete. This seems
       ;; to mean that the region has changed arbitrarily. The only
       ;; option we have is sending a delete and corresponding insert
       ;; message that emulate the change.
       (t
	(with-slots (buffer) document
	  (with-current-buffer buffer
	    (setq text (buffer-substring-no-properties from to)))
	  (rudel-local-operation document
				 (rudel-delete-op
				  "delete"
				  :from   (- from 1)
				  :length length))
	  (rudel-local-operation document
				 (rudel-insert-op
				  "insert"
				  :from (- from 1)
				  :data text)))))))
  )

(defun rudel-buffer-change-workaround (from to)
  (when (/= from to)
    (setq rudel-buffer-change-workaround-data
	  (list from to
		(buffer-substring-no-properties from to)))))


;;; Protection against major mode changes
;;

(defvar rudel-mode-changed-buffers nil
  "List of buffers that may need to be repaired after a major
  mode change.")

(defun rudel-handle-major-mode-change ()
  "Store the current buffer to repair damage done by major mode change.

Note: The way this works is inspired by mode-local.el by David
Ponce and Eric M. Ludlam."
  ;; Store the buffer for later repair.
  (add-to-list 'rudel-mode-changed-buffers (current-buffer))

  ;; Schedule `rudel-after-major-mode-change' to run after the
  ;; command, that caused the major mode change.
  (add-hook 'post-command-hook
	    #'rudel-after-major-mode-change)
  )

(defun rudel-after-major-mode-change ()
  "Repair damage done by major mode changes.
As a function in `post-command-hook', this is run after there was
a `major-mode' change.

Note: The way this works is inspired by mode-local.el by David
Ponce and Eric M. Ludlam."
  ;; Remove this function from `post-command-hook'.
  (remove-hook 'post-command-hook
	       #'rudel-after-major-mode-change)

  ;; Repair all buffers affected by the major mode change.
  (dolist (buffer rudel-mode-changed-buffers)
    (let ((document (buffer-local-value 'rudel-buffer-document
					buffer)))
      (rudel-attach-to-buffer document buffer)))
  )


;;; Interactive functions
;;

(defun rudel-join-session (info)
  "Join the collaborative editing session described by INFO.
INFO is a property list that describes the collaborative editing
session in terms of properties like :host, :port
and :encryption. The particular properties and their respective
meanings depend on the used backend.

When called interactively, all data required to join a session
will be prompted for."
  (interactive
   ;; Try the discover method of session initiation backends to find
   ;; available sessions.
   (list
    (let ((info)
	  (session-initiation-backend))
      (while (not info)
	(message "Discovering Sessions ...")
	(let* ((sessions   (rudel-session-initiation-discover
			    session-initiation-backend))
	       (maybe-info (if (= (length sessions) 1)
				 (car sessions)
			       (rudel-read-session
				sessions "Choose Session: " 'object))))
	  (if (rudel-backend-cons-p maybe-info)
	      (setq session-initiation-backend (car maybe-info))
	    (setq info maybe-info))))
    info)))

  ;; First, create the session object.
  (let* ((backend    (cdr (plist-get info :backend)))
	 (session    (rudel-client-session
		      (plist-get info :name)
		      :backend backend))
	 (connection))
    ;; Give the backend a chance to collect remaining connect
    ;; info. For session initiation methods like Zeroconf, we have the
    ;; _connection_ info, but are still missing the username and
    ;; stuff.
    (setq info (rudel-ask-connect-info backend info))

    ;; Add the session object to the connect information.
    (plist-put info :session session)

    ;; Ask BACKEND to connect using INFO. Do not catch errors since
    ;; the error messages are probably the best feedback we can give.
    (setq connection (rudel-connect backend info))

    ;; Set the connection slot of the session object and store it
    ;; globally.
    (oset session :connection connection)
    (setq rudel-current-session session)

    ;; Reset the global session variable when the session ends.
    (object-add-hook session 'end-hook
		     (lambda (session)
		       (setq rudel-current-session nil)))

    ;; Run the hook.
    (run-hook-with-args 'rudel-session-start-hook session))
  )

(defun rudel-host-session ()
  "Host a collaborative editing session.
All data required to host a session will be prompted for
interactively."
  (interactive)
  ;; If necessary, ask the user for the backend we should use.
  (let* ((backend (cdr (rudel-backend-choose
			'protocol
			(lambda (backend)
			  (rudel-capable-of-p backend 'host)))))
	 (info    (rudel-ask-host-info backend))
	 (server))

    ;; Try to create the server
    (condition-case error-data
	(setq server (rudel-host backend info))
      ('error
       (error "Could not host session using backend `%s' with %s: %s"
	      (object-name-string backend)
	      info
	      (car error-data))))
    server))

(defun rudel-end-session ()
  "End the current collaborative editing session."
  (interactive)
  (unless rudel-current-session
    (error "No active Rudel session"))

  ;; Actually end the session.
  (rudel-end rudel-current-session)
  )

(defun rudel-change-color ()
  "Change the color associated with the local user.
Not all backends support this operation."
  (interactive)
  ;; Make sure we have a session.
  (unless rudel-current-session
    (error "No active Rudel session"))

  (with-slots (backend connection self) rudel-current-session
    ;; Make sure the backend can change colors.
    (unless (rudel-capable-of-p backend 'change-color)
      (error "Backend `%s' cannot change colors"
	     (object-name-string backend)))

    (with-slots ((name :object-name) color) self
      ;; Ask the user for a new color.
      (setq color (read-color "New Color: " t))

      ;; Tell the connection to announce the change and change it in
      ;; our user object.
      (rudel-change-color- connection color)

      ;; Run the change hook.
      (object-run-hook-with-args self 'change-hook)

      ;; Update overlay color.
      (rudel-overlay-set-face-attributes
       (rudel-overlay-make-face-symbol 'author name)
       color)))
  )

(defun rudel-subscribe (document)
  "Subscribe to DOCUMENT offered by a peer in a collaborative editing session.
When called interactively, DOCUMENT is prompted for interactively."
  (interactive
   (list (progn
	   ;; We have to retrieve the document list from an active
	   ;; session.
	   (unless rudel-current-session
	     (error "No active Rudel session"))
	   ;; Select unsubscribed documents.
	   (let ((documents (rudel-unsubscribed-documents
			     rudel-current-session)))
	     ;; Already subscribed to all documents. This is an error.
	     (when (null documents)
	       (error "No unsubscribed documents"))
	     ;; Read an unsubscribed document.
	     (rudel-read-document documents nil 'object)))))

  ;; Make sure we have a session.
  (unless rudel-current-session
    (error "No active Rudel session"))

  ;; Create a new buffer and attach the document to it.
  (let* ((name   (rudel-suggested-buffer-name document))
	 (buffer (funcall rudel-allocate-buffer-function name)))
    (rudel-attach-to-buffer document buffer)

    (let ((connection (oref (oref document :session) :connection)))
      (rudel-subscribe-to connection document))

    ;; Show the new buffer.
    (set-window-buffer nil buffer))
  )

(defun rudel-publish-buffer (&optional buffer)
  "Make the BUFFER available for subscription to peers in a collaborative editing session.
If BUFFER is nil, the current buffer is used."
  (interactive (list nil))

  ;; Make sure we have a session.
  (unless rudel-current-session
    (error "No active Rudel session"))

  (unless buffer
    (setq buffer (current-buffer)))

  (with-current-buffer buffer
    (when (rudel-buffer-has-document-p)
      (error "Buffer already published or subscribed"))) ; TODO keep this?

  ;;
  (with-slots (backend connection self) rudel-current-session
    (let ((document (rudel-make-document backend
					 (buffer-name buffer)
					 rudel-current-session)))
      (rudel-add-document rudel-current-session document)

      (rudel-attach-to-buffer document buffer)
      (object-add-to-list document :subscribed self)

      (rudel-publish connection document)))
  )

(defun rudel-unpublish-buffer (&optional buffer)
  "Deny peers access to BUFFER in a collaborative editing session.
If BUFFER is nil, the current is used."
  (interactive)

  ;; Make sure we have a session.
  (unless rudel-current-session
    (error "No active Rudel session"))

  (unless buffer
    (setq buffer (current-buffer)))

  (with-current-buffer buffer
    (unless (rudel-buffer-has-document-p)
      (error "Buffer is not published")))

  ;;
  (with-slots (connection) rudel-current-session
    (let ((document (rudel-buffer-document buffer)))
      (rudel-detach-from-buffer document)

      (rudel-unsubscribe-from connection document)))
  )

(provide 'rudel)
;;; rudel.el ends here

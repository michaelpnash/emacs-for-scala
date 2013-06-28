;;; rudel-debug.el --- Debugging functions for Rudel
;;
;; Copyright (C) 2009 Jan Moringen
;;
;; Author: Jan Moringen <scymtym@users.sourceforge.net>
;; Keywords: Rudel, debugging
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
;; Debugging functions for Rudel.


;;; History:
;;
;; 0.1 - Initial revision.


;;; Code:
;;

(require 'data-debug)
(require 'eieio-datadebug)

(require 'rudel-util)


;;; Customization
;;

(defgroup rudel-debug nil
  "Customization options related to Rudel's debugging functions."
  :group 'rudel)

(defface rudel-debug-sent-data-face
  '((default (:background "orange")))
  "Face used for sent data."
  :group 'rudel-debug)

(defface rudel-debug-received-data-face
  '((default (:background "light sky blue")))
  "Face used for received (but not yet processed) data."
  :group 'rudel-debug)

(defface rudel-debug-received-processed-data-face
  '((default (:background "DeepSkyBlue1")))
  "Face used for received data after processing."
  :group 'rudel-debug)

(defface rudel-debug-state-face
  '((default (:background "light gray")))
  "Face used when indicating state changes."
  :group 'rudel-debug)

(defface rudel-debug-special-face
  '((default (:background "light sea green")))
  "Face used for additional information."
  :group 'rudel-debug)

(defvar rudel-debug-tag-faces
  '((:sent               . (rudel-debug-sent-data-face               "<  "))
    (:received           . (rudel-debug-received-data-face           ">  "))
    (:received-processed . (rudel-debug-received-processed-data-face ">> "))
    (:state              . (rudel-debug-state-face                   "|  "))
    (:special            . (rudel-debug-special-face                 ";  ")))
  "Associate tag to faces and prefixes.")


;;; Data debug functions
;;

(defun rudel-adebug-session ()
  "Analyze current session in data debug buffer."
  (interactive)

  ;; Make sure we have a session.
  (unless rudel-current-session
    (error "No active Rudel session"))

  (with-current-buffer (data-debug-new-buffer "RUDEL-SESSION")
    (data-debug-insert-thing rudel-current-session "# " "")))

(defun rudel-adebug-server (server)
  "Analyze server in data debug buffer."
  (interactive)

  (with-current-buffer (data-debug-new-buffer "RUDEL-SERVER")
    (data-debug-insert-thing server "# " "")))


;;; Advice stuff
;;

(defadvice rudel-join-session (after rudel-debug last activate)
  "Run data-debug inspection on newly created session objects."
  (require 'rudel-debug)
  (rudel-adebug-session))

(defadvice rudel-host-session (after rudel-debug last activate)
  "Run data-debug inspection on newly created server objects."
  (require 'rudel-debug)
  (rudel-adebug-server ad-return-value))


;;; Network functions
;;

(defun rudel-suspend-session-socket ()
  "Suspend the socket associated to the current session."
  (interactive)

  ;; Make sure we have a session.
  (unless rudel-current-session
    (error "No active Rudel session"))

  (with-slots (connection) rudel-current-session
    (with-slots (socket) connection
      (stop-process socket))))

(defun rudel-resume-session-socket ()
  "Resume the socket associated to the current session."
  (interactive)

  ;; Make sure we have a session.
  (unless rudel-current-session
    (error "No active Rudel session"))

  (with-slots (connection) rudel-current-session
    (with-slots (socket) connection
      (continue-process socket))))


;;; Reset functions
;;

(defun rudel-kill-processes ()
  "TODO"
  (interactive)
  (mapc #'delete-process (process-list)))

(defun rudel-reset ()
  "TODO"
  (interactive)
  (dolist (buffer (buffer-list))
    (with-current-buffer buffer
      (when rudel-buffer-document
	(setq rudel-buffer-document nil))))
  (rudel-kill-processes)
  (setq rudel-current-session nil))


;;; Socket debugging
;;

(defmethod rudel-state-change :before ((this rudel-socket-owner)
				       state message)
  "Print STATE and MESSAGE to debug stream."
  (with-slots (socket) this
    (rudel-debug-stream-insert
     (rudel-debug-stream-name socket)
     :state
     (format "connection state changed to %s: \"%s\""
	     (upcase (symbol-name state))
	     ;; MESSAGE ends with a newline; remove it
	     (substring message 0 -1))))
  )


;;; Utility functions
;;

(defun rudel-debug-stream-name (socket)
  "Return debug stream name for SOCKET."
  (process-name socket))

(defun rudel-debug-stream-insert (stream tag data &optional object)
  "Insert DATA and possibly OBJECT into stream using TAG as style."
  (let* ((buffer-name (format "*%s stream*" stream))
	 (buffer      (or (get-buffer buffer-name)
			  (data-debug-new-buffer buffer-name)))
	 (appearance (cdr (assoc tag rudel-debug-tag-faces)))
	 (face       (when appearance
		       (or (nth 0 appearance)
			   'default)))
	 (prefix     (or (nth 1 appearance)
			 "")))
    (save-excursion
      (set-buffer buffer)
      (goto-char 0)
      (insert prefix
	      (propertize data 'face face)
	      (if (string= (substring data -1) "\n")
		  "" "\n"))
      (when object
	(data-debug-insert-thing object prefix ""))))
    )

(provide 'rudel-debug)
;;; rudel-debug.el ends here

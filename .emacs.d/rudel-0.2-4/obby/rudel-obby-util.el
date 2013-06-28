;;; rudel-obby-util.el --- Miscellaneous functions for the Rudel obby backend
;;
;; Copyright (C) 2008, 2009 Jan Moringen
;;
;; Author: Jan Moringen <scymtym@users.sourceforge.net>
;; Keywords: Rudel, obby, backend, miscellaneous
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
;;


;;; History:
;;
;; 0.1 - Initial revision.


;;; Code:
;;

(eval-when-compile
  (require 'cl))

(require 'eieio)

(require 'rudel)
(require 'rudel-util)

(require 'jupiter)


;;; Class rudel-obby-socket-owner
;;

(defclass rudel-obby-socket-owner (rudel-socket-owner)
  ((buffer :initarg  :buffer
	   :type     (or null string)
	   :initform nil
	   :documentation
	   "Stores message fragments until complete messages can
be assembled."))
  "This class adds functions for sending and receiving obby
messages to the base class rudel-socket-owner.")

(defmethod rudel-send ((this rudel-obby-socket-owner)
		       name &rest arguments)
  "Send obby message NAME with arguments ARGUMENTS through the socket associated to THIS."
  (with-slots (socket) this
    (rudel-obby-send socket name arguments)))

(defmethod rudel-receive ((this rudel-obby-socket-owner) data)
  "Reassemble lines in DATA received on the socket associated with THIS and call message handler."
  ;; Assemble fragmented lines.
  (with-slots (buffer) this
    (rudel-assemble-line-fragments data buffer))

  ;; Process all available lines.
  (rudel-loop-lines data line
    ;; `rudel-message' has to dispatch message to an appropriate
    ;; handler.
    (let ((message (rudel-obby-parse-message line)))
      (rudel-message this message)))
  )

(defgeneric rudel-message ((this rudel-obby-socket-owner) message)
  "Called when a message arrives.
Should be implemented in derived classes.")


;;; Message serialization
;;

(defgeneric rudel-operation->message ((this jupiter-operation))
  "Generate a list obby message components from THIS operation.")

(defmethod rudel-operation->message ((this jupiter-insert))
  "Serialize THIS insert operation."
  (with-slots (from data) this
    (list "ins" (format "%x" from) data)))

(defmethod rudel-operation->message ((this jupiter-delete))
  "Serialize THIS delete operation."
  (with-slots (from length) this
    (list "del" (format "%x" from) (format "%x" length))))

(defmethod rudel-operation->message ((this jupiter-compound))
  "Serialize THIS compound operation."
  (with-slots (children) this
    (apply #'append
	   (list "split" )
	   (mapcar #'rudel-operation->message children))))

(defmethod rudel-operation->message ((this jupiter-nop))
  "Serialize THIS nop operation."
  (list "nop"))

(defun rudel-message->operation (message local-revision remote-revision)
  "Construct an operation object from MESSAGE and LOCAL-REVISION and REMOTE-REVISION.
LOCAL-REVISION and REMOTE-REVISION are only used in the
construction of the name of the new operation. "
  (let ((type (car message)))
    (cond

     ;; Insert operation
     ((string= type "ins")
      (let ((position-numeric (string-to-number (nth 1 message) 16))
	    (data             (nth 2 message)))
	(jupiter-insert
	 (format "insert-%d-%d"
		 remote-revision local-revision)
	 :from position-numeric
	 :data data)))

     ;; Delete operation
     ((string= type "del")
      (let ((position-numeric (string-to-number (nth 1 message) 16))
	    (length-numeric   (string-to-number (nth 2 message) 16)))
	(jupiter-delete
	 (format "delete-%d-%d"
		 remote-revision local-revision)
	 :from position-numeric
	 :to   (+ position-numeric length-numeric))))

     ;; Compound operation
     ((string= type "split")
      (let* ((rest   (cdr message))
	     (offset (position-if
		       (lambda (item)
			 (member* item '("ins" "del" "nop")
				  :test #'string=))
		       rest
		       :start 1))
	     (first  (subseq rest 0 offset))
	     (second (subseq rest offset)))
	(jupiter-compound
	 (format "compound-%d-%d"
		 remote-revision local-revision)
	 :children
	 (list (rudel-message->operation
		first local-revision remote-revision)
	       (rudel-message->operation
		second local-revision remote-revision)))))

     ;; No operation
     ((string= type "nop")
      (jupiter-nop
       (format "nop-%d-%d"
	       remote-revision local-revision)))

     ;; Unknown operation type
     (t (error "Unknown document record type: `%s'" type))))
  )


;;; Character <-> byte position conversion
;;

(defgeneric rudel-obby-char->byte ((this jupiter-operation) buffer)
  "Convert character positions and lengths in THIS to bytes.")

(defmethod rudel-obby-char->byte ((this jupiter-insert) buffer)
  "Convert character positions and lengths in THIS insert to bytes."
  (with-slots (from) this
    (with-current-buffer buffer
      (setq from (- (position-bytes (+ from 1)) 1)))))

(defmethod rudel-obby-char->byte ((this jupiter-delete) buffer)
 "Convert character positions and lengths in THIS delete to bytes."
  (with-slots (from to length) this
    (let ((old-from (+ from 1))
	  (old-to   (+ to   1)))
      (with-current-buffer buffer
	(destructuring-bind (change-from change-to string)
	    rudel-buffer-change-workaround-data
	  (setq from   (- (position-bytes old-from) 1)
		length (string-bytes
			(substring string
				   (- old-from change-from)
				   (- old-to   change-from))))))))
  )

(defmethod rudel-obby-char->byte ((this jupiter-compound) buffer)
  "Convert character positions and lengths in THIS compound to bytes.."
  (with-slots (children) this
    (mapc
     (lambda (child)
       (rudel-obby-char->byte child buffer))
     children))
  )

(defmethod rudel-obby-char->byte ((this jupiter-nop) buffer)
  "Nothing to convert if THIS is a nop.")

(defgeneric rudel-obby-byte->char ((this jupiter-operation) buffer)
  "Convert byte positions and lengths in THIS to character positions.")

(defmethod rudel-obby-byte->char ((this jupiter-insert) buffer)
  "Convert byte positions and lengths in THIS insert to character positions."
  (with-slots (from) this
    (with-current-buffer buffer
      (setq from (- (byte-to-position (+ from 1)) 1)))))

(defmethod rudel-obby-byte->char ((this jupiter-delete) buffer)
  "Convert byte positions and lengths in THIS delete to character positions."
  (with-slots (from to length) this
    (let ((old-from   from)
	  (old-length length))
      (with-current-buffer buffer
	(setq from (- (byte-to-position (+ old-from 1)) 1)
	      to   (- (byte-to-position (+ old-from old-length 1)) 1)))))
  )

(defmethod rudel-obby-byte->char ((this jupiter-compound) buffer)
  "Convert byte positions and lengths in THIS compound to character positions."
  (with-slots (children) this
    (mapc
     (lambda (child)
       (rudel-obby-byte->char child buffer))
     children))
  )

(defmethod rudel-obby-byte->char ((this jupiter-nop) buffer)
  "Nothing to convert if THIS is a nop.")


;;; Miscellaneous functions
;;

(defun rudel-obby-dispatch (object name arguments &optional prefix)
  "Call method starting with PREFIX and ending in NAME of OBJECT with ARGUMENTS.
When PREFIX is not specified, \"rudel-obby/\" is used."
  ;; Default prefix.
  (unless prefix
    (setq prefix "rudel-obby/"))

  ;; Construct a matching symbol.
  (let ((method (intern-soft (concat prefix name))))
    ;; If we found a suitable method, run it; Otherwise warn and do
    ;; nothing.
    (unless (and method
		 (condition-case error
		     ;; Try to call METHOD. If successful, always
		     ;; return t.
		     (progn
		       (apply method object arguments)
		       t)
		   ;; Warn only when the condition is
		   ;; 'no-method-definition' and refers to METHOD,
		   ;; otherwise continue unwinding.
		   (no-method-definition
		    (if (eq method (cadr error))
			nil
		      (signal (car error) (cdr error))))))
      (display-warning
       '(rudel obby)
       (format "%s: in context `%s': no method: `%s'; arguments: %s"
	       (object-name-string object) prefix name arguments)
       :debug)))
  )

(defmacro with-parsed-arguments (specs &rest forms)
  "Execute FORMS with variable bindings according to SPECS.
SPECS is structured as follows:
SPECS   ::= (BINDING*)
BINDING ::= (VAR TYPE)
VAR is a symbol and TYPE is one of number, color, document-id and
coding-system."
  (declare (indent 1)
	   (debug (listp &rest form)))
  (let ((bindings
	 (mapcar
	  (lambda (spec)
	    (destructuring-bind (var type) spec
	      (list var
		    (case type
		      ;; Number
		      (number
		       `(string-to-number ,var 16))
		      ;; Color
		      (color
		       `(rudel-obby-parse-color ,var))
		      ;; Document Id
		      (document-id
		       `(mapcar
			 (lambda (string)
			   (string-to-number string 16))
			 (split-string ,var " " t)))
		      ;; Coding System
		      (coding-system
		       `(rudel-get-coding-system (downcase ,var)))))))
	  specs)))
    `(let (,@bindings)
       ,@forms))
  )

(provide 'rudel-obby-util)
;;; rudel-obby-util.el ends here

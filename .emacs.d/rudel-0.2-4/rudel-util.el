;;; rudel-util.el --- Miscellaneous functions for Rudel
;;
;; Copyright (C) 2008, 2009 Jan Moringen
;;
;; Author: Jan Moringen <scymtym@users.sourceforge.net>
;; Keywords: rudel, miscellaneous, util
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
;; This file contains miscellaneous functions for Rudel.


;;; History:
;;
;; 0.1 - Initial revision.


;;; Code:
;;

(eval-when-compile
  (require 'cl))

(require 'eieio)

(require 'rudel-errors)


;;; Errors
;;

;; rudel-dispatch-error

(intern "rudel-dispatch-error")

(put 'rudel-dispatch-error 'error-conditions
     '(error
       rudel-error rudel-dispatch-error))

(put 'rudel-dispatch-error 'error-message
     "Could not dispatch message to handler")


;;; Class rudel-hook-object
;;

(defclass rudel-hook-object ()
  ()
  "Mixin for classes that offer one or more hooks for each of
their objects.

This idiom is usually called something like signal/slot or
event/subscription, but for Emacs, the notion of hooks seems more
appropriate."
  :abstract t)

(defmethod object-add-hook ((this rudel-hook-object)
			    hook function &optional append)
  "Add FUNCTION to HOOK for THIS.
If APPEND is non-nil FUNCTION becomes the last element in the
list of hooks."
  (let ((value (slot-value this hook)))
    (unless (member function value)
      (set-slot-value this hook
		      (if append (append value (list function))
			(cons function value)))))
  )

(defmethod object-remove-hook ((this rudel-hook-object)
			       hook function)
  "Remove FUNCTION from HOOK for THIS."
  (set-slot-value this hook
		  (remove function (slot-value this hook))))

(defmethod object-run-hook-with-args ((this rudel-hook-object)
				      hook &rest arguments)
  "Run HOOK of THIS with arguments ARGUMENTS."
  (let ((hook (slot-value this hook)))
    (apply #'run-hook-with-args 'hook this arguments)))


;;; Class rudel-socket-owner
;;

(defclass rudel-socket-owner ()
  ((socket :initarg :socket
	   :type    process
	   :documentation
	   "The process object representing the socket through
which the communication happens."))
  "Class rudel-socket-owner ")

(defmethod initialize-instance :after ((this rudel-socket-owner)
				       &rest slots)
  "Attach THIS to as process object of our socket."
  ;; Attach to our socket.
  (with-slots (socket) this
    (rudel-set-process-object socket this))
  )

(defmethod rudel-disconnect ((this rudel-socket-owner))
  "Disconnect the network connection owned by THIS."
  (with-slots (socket) this
    (delete-process socket)))

(defmethod rudel-state-change ((this rudel-socket-owner) state message)
  "Called when the state of THIS changes to STATE.
MESSAGE is the message emitted when the state transition
occurred."
  (with-slots (socket) this
    (case state
      ;; Nothing to do here.
      (run
       nil)

      ;; Dispatch events which indicate the termination of the
      ;; connection to `rudel-close'.
      ((closed failed exit)
       (rudel-close this))))
  )

(defmethod rudel-close ((this rudel-socket-owner))
  "Called when the connection associated to THIS is closed.")


;;; Networking helper functions and macros
;;

(defun rudel-process-object (process &optional key)
  "Return the object attached to PROCESS using identifier KEY."
  (unless key
    (setq key :object))
  (get (intern (process-name process)) key))

(defun rudel-set-process-object (process object &optional key)
  "Set object attached to PROCESS using identifier KEY to OBJECT."
  (unless key
    (setq key :object))
  (put (intern (process-name process)) key object))

(defun rudel-filter-dispatch (process data)
  "Call `rudel-receive' method of object attached to PROCESS with DATA."
  (let ((object (rudel-process-object process)))
    (rudel-receive object data)))

(defun rudel-sentinel-dispatch (process message)
  "Call `rudel-state-change' method of the object attached to PROCESS with state and MESSAGE."
  (let ((object (rudel-process-object process))
	(state  (process-status process)))
    (rudel-state-change object state message)))


;;; Fragmentation and assembling functions.
;;

(defmacro rudel-assemble-line-fragments (data storage)
  "Find an return complete lines in DATA, store excess data in STORAGE.
If STORAGE is non-nil when calling, consider content as leftover
data from last and concatenate with DATA before processing."
  (declare (debug (form form)))
  (let ((index (make-symbol "index")))
    `(progn
       ;; If there are stored fragments, append them to the new data.
       (when ,storage
	 (setq ,data    (concat ,storage ,data))
	 (setq ,storage nil))
       ;; Try to find a line break in the augmented data.
       (let ((,index (position ?\n ,data :from-end t)))
	 (unless (and ,index (eq ,index (- (length ,data) 1)))
	   (setq ,storage (if ,index
			      (substring ,data (+ ,index 1))
			    ,data))
	   (setq ,data    (when ,index
			    (substring ,data 0 (+ ,index 1))))))
       ,data))
  )

(defmacro rudel-loop-lines (data var &rest forms)
  "Execute FROMS with VAR subsequently bound to all lines in DATA."
  (declare (indent 2)
	   (debug (form symbolp &rest form)))
  (let ((lines (make-symbol "lines")))
    `(when ,data
       (let ((,lines (split-string ,data "\n" t)))
	 (dolist (,var ,lines)
	   (progn ,@forms)))))
  )

(defmacro rudel-loop-chunks (data var size &rest forms)
  "Execute FROMS in a loop with VAR bound to chunks of DATA of SIZE.
Unless (zerop (mod (length data) size) 0) the final chunk is
truncated. The expression SIZE is evaluated in each loop unless
it is a number."
  (declare (indent 3)
	   (debug (form symbolp numberp &rest form)))
  ;; If we got a constant number as SIZE, we can check right away.
  (when (and (numberp size) (<= size 0))
    (error "Size should be positive"))

  (let ((rest   (make-symbol "rest"))
	(amount (make-symbol "amount"))
	;; If SIZE has to be evaluated, we have to check at runtime.
	(check  (unless (numberp size)
		  `((when (<= ,size 0)
		      (error "Size should be positive"))))))
    `(progn
       ,@check
       (let ((,rest ,data)
	     (,var)
	     (,amount))
	 (while (not (string= ,rest ""))
	   (setq ,amount (min (length ,rest) ,size)
		 ,var    (substring ,rest 0 ,amount)
		 ,rest   (substring ,rest ,amount))
	   (progn ,@forms)))))
  )


;;; Miscellaneous functions
;;

(defun rudel-dispatch (object prefix name arguments)
  "Call method (concat PREFIX NAME) of OBJECT with ARGUMENTS.
If no such method can be found, the condition
rudel-dispatch-error is signalled."
  ;; Construct a matching symbol.
  (let* ((method (intern-soft (concat prefix name))))
    ;; If we found a suitable method, run it; Otherwise signal.
    (unless method
      (signal 'rudel-dispatch-error 'method-symbol-unbound))
    (condition-case error
	;; Try to call METHOD. This can still fail when METHOD is not
	;; defined for the class of OBJECT.
	(apply method object arguments)
      ;; Only handle a condition 'no-method-definition' that refers to
      ;; METHOD, otherwise continue unwinding.
      (no-method-definition
       (if (eq method (cadr error))
	   (signal 'rudel-dispatch-error 'no-method-for-object)
	 (signal (car error) (cdr error))))))
  )

(provide 'rudel-util)
;;; rudel-util.el ends here

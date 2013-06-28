;;; rudel-operations.el --- Rudel domain operations
;;
;; Copyright (C) 2009 Jan Moringen
;;
;; Author: Jan Moringen <scymtym@users.sourceforge.net>
;; Keywords: Rudel, operations
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
;; This file contains classes representing operations like insertions
;; and deletions, that are import for Rudel's domain of collaborative
;; editing.


;;; History:
;;
;; 0.1 - Initial revision


;;; Code:
;;

(require 'eieio)


;;; Class rudel-operation
;;

(defclass rudel-operation ()
  ()
  "Abstract base class for operations."
  :abstract t)

(defgeneric rudel-apply ((this rudel-operation) object)
  "Apply the change represented by THIS to OBJECT.")


;;; Class rudel-insert-op
;;

;; TODO should be rudel-insert but there is a method of the same name
(defclass rudel-insert-op (rudel-operation)
  ((from :initarg :from
	 :type    (or null (integer 0))
	 :documentation
	 "Start of the region affected by this operation or
nil. nil means end of buffer")
   (data :initarg :data
	 :type    string
	 :documentation
	 "The inserted string."))
  "Objects of this class represent insertion operations.")

(defmethod rudel-apply ((this rudel-insert-op) object)
  "Apply THIS to OBJECT by inserting the associated data."
  (with-slots (from data) this
    (rudel-insert object from data)))

(defmethod slot-missing ((this rudel-insert-op)
			 slot-name operation &optional new-value)
  "Simulate read-only slots :length and :to."
  (cond
   ;; Slot :length
   ((and (or (eq slot-name :length)
	     (eq slot-name 'length))
	 (eq operation 'oref))
    (with-slots (data) this
      (length data)))
   ;; Slot :to
   ((and (or (eq slot-name :to)
	     (eq slot-name 'to))
	 (eq operation 'oref))
    (with-slots (from length) this
      (+ from length)))
   ;; Call next method
   (t (call-next-method)))
  )


;;; Class rudel-delete-op
;;

;; TODO should be rudel-delete but there is a method of the same name
(defclass rudel-delete-op (rudel-operation)
  ((from :initarg :from
	 :type    (integer 0)
	 :documentation
	 "Start of the region affected by this operation.")
   (to   :initarg :to
	 :type    (integer 0)
	 :documentation
	 "End of the region affected by this operation."))
  "Objects of this class represent deletion operations.")

(defmethod rudel-apply ((this rudel-delete-op) object)
  "Apply THIS to OBJECT by deleting the associated region."
  (with-slots (from length) this
    (rudel-delete object from length)))

(defmethod slot-missing ((this rudel-delete-op)
			 slot-name operation &optional new-value)
  "Simulate slot :length"
  (cond
   ;; Slot :length
   ((or (eq slot-name :length)
	(eq slot-name 'length))
    (with-slots (from to) this
      (if (eq operation 'oref)
	  (- to from)
	(setq to (+ from new-value)))))
   ;; Call next method
   (t (call-next-method)))
  )

(provide 'rudel-operations)
;;; rudel-operations.el ends here

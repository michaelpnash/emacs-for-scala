;;; rudel-speedbar.el --- Speedbar rendering of Rudel objects
;;
;; Copyright (C) 2008, 2009 Jan Moringen
;;
;; Author: Jan Moringen <scymtym@users.sourceforge.net>
;; Keywords: rudel, collaboration, speedbar
;; X-RCS: $Id: rudel-speedbar.el,v 1.32 2008/10/10 21:47:28 zappo Exp $
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
;; This implements rendering of Rudel objects in speedbar.


;;;  History:
;;
;; 0.1 - Initial revision.


;;; Code:
;;

(require 'speedbar)
(require 'eieio-speedbar)


;;; Class rudel-user methods
;;

(defmethod eieio-speedbar-description ((this rudel-user))
  "Provide a speedbar description for OBJ."
  (format "User %s" (object-name-string this)))

(defmethod eieio-speedbar-object-buttonname ((this rudel-user))
  "Return a string to use as a speedbar button for OBJECT."
  (format "%s" (object-name-string this)))


;;; Class rudel-document methods
;;

(defmethod eieio-speedbar-description ((this rudel-document))
  "Construct a description for from the name of document object THIS."
  (format "Document %s" (object-name-string this)))

(defmethod eieio-speedbar-object-buttonname ((this rudel-document))
  "Return a string to use as a speedbar button for OBJECT."
  (rudel-unique-name this))

;;; Speedbar support mode
;;
(defun rudel-speedbar-make-map ()
  "Make the generic object based speedbar keymap."
  (speedbar-make-specialized-keymap))

(defvar rudel-speedbar-key-map
  (rudel-speedbar-make-map)
  "A Generic object based speedbar display keymap.")

(defvar rudel-speedbar-menu
  '([ "Compile" rudel-speedbar-compile-line t])
  "Menu part in easymenu format used in speedbar while browsing objects.")

(defun rudel-speedbar-toplevel-buttons (dir)
  "Return a list of objects to display in speedbar.
Argument DIR is the directory from which to derive the list of objects."
  (when rudel-current-session
    (with-slots (users documents) rudel-current-session
      (append users documents))))

(eieio-speedbar-create 'rudel-speedbar-make-map
		       'rudel-speedbar-key-map
		       'rudel-speedbar-menu
		       "Collaboration Session"
		       'rudel-speedbar-toplevel-buttons)

(defun rudel-speedbar ()
  "Show connected users and available documents of Rudel session in speedbar."
  (interactive)
  (speedbar-frame-mode 1)
  (speedbar-change-initial-expansion-list "Collaboration Session")
  (speedbar-get-focus))

;;; Speedbar Project Methods
;;
;; (defun rudel-find-nearest-file-line ()
;;   "Go backwards until we find a file."
;;   (save-excursion
;;     (beginning-of-line)
;;     (looking-at "^\\([0-9]+\\):")
;;     (let ((depth (string-to-number (match-string 1))))
;;       (while (not (re-search-forward "[]] [^ ]"
;; 				     (save-excursion (end-of-line)
;; 						     (point))
;; 				     t))
;; 	(re-search-backward (format "^%d:" (1- depth)))
;; 	(setq depth (1- depth)))
;;       (speedbar-line-token))))
;;
;; (defmethod eieio-speedbar-derive-line-path ((obj rudel-session) &optional depth)
;;   "Return the path to OBJ.
;; Optional DEPTH is the depth we start at."
;;   "session" ;(file-name-directory (oref obj file))
;;   )
;;
;; (defmethod eieio-speedbar-derive-line-path ((obj rudel-session) &optional depth)
;;   "Return the path to OBJ.
;; Optional DEPTH is the depth we start at."
;;   (let ((proj (rudel-target-parent obj)))
;;     ;; Check the type of line we are currently on.
;;     ;; If we are on a child, we need a file name too.
;;     (save-excursion
;;       (let ((lt (speedbar-line-token)))
;; 	(if (or (eieio-object-p lt) (stringp lt))
;; 	    (eieio-speedbar-derive-line-path proj)
;; 	  ;; a child element is a token.  Do some work to get a filename too.
;; 	  (concat (eieio-speedbar-derive-line-path proj)
;; 		  (rudel-find-nearest-file-line)))))))
;;
;; (defmethod eieio-speedbar-description ((obj rudel-session))
;;   "Provide a speedbar description for OBJ."
;;   "bla") ;(rudel-description obj))
;;
;;
;; (defmethod eieio-speedbar-object-buttonname ((object rudel-project))
;;   "Return a string to use as a speedbar button for OBJECT."
;;   (if (rudel-parent-project object)
;;       (rudel-name object)
;;     (concat (rudel-name object) " " (oref object version))))
;;
;;
;; (defmethod eieio-speedbar-object-children ((this rudel-project))
;;   "Return the list of speedbar display children for THIS."
;;   (condition-case nil
;;       (with-slots (subproj targets) this
;; 	(append subproj targets))
;;     (error nil)))
;;
;;
;; ;;; Generic file management for TARGETS
;; ;;
;; (defun rudel-file-find (text token indent)
;;   "Find the file TEXT at path TOKEN.
;; INDENT is the current indentation level."
;;   (speedbar-find-file-in-frame
;;    (expand-file-name token (speedbar-line-directory indent)))
;;   (speedbar-maybee-jump-to-attached-frame))
;;
;; (defun rudel-create-tag-buttons (filename indent)
;;   "Create the tag buttons associated with FILENAME at INDENT."
;;   (let* ((lst (speedbar-fetch-dynamic-tags filename)))
;;     ;; if no list, then remove expando button
;;     (if (not lst)
;; 	(speedbar-change-expand-button-char ??)
;;       (speedbar-with-writable
;; 	;; We must do 1- because indent was already incremented.
;; 	(speedbar-insert-generic-list (1- indent)
;; 				      lst
;; 				      'rudel-tag-expand
;; 				      'rudel-tag-find)))))
;;
;; (defun rudel-tag-expand (text token indent)
;;   "Expand a tag sublist.  Imenu will return sub-lists of specialized tag types.
;; Etags does not support this feature.  TEXT will be the button
;; string.  TOKEN will be the list, and INDENT is the current indentation
;; level."
;;   (cond ((string-match "+" text)	;we have to expand this file
;; 	 (speedbar-change-expand-button-char ?-)
;; 	 (speedbar-with-writable
;; 	   (save-excursion
;; 	     (end-of-line) (forward-char 1)
;; 	     (speedbar-insert-generic-list indent token
;; 					   'rudel-tag-expand
;; 					   'rudel-tag-find))))
;; 	((string-match "-" text)	;we have to contract this node
;; 	 (speedbar-change-expand-button-char ?+)
;; 	 (speedbar-delete-subblock indent))
;; 	(t (error "Ooops...  not sure what to do")))
;;   (speedbar-center-buffer-smartly))
;;
;; (defun rudel-tag-find (text token indent)
;;   "For the tag TEXT in a file TOKEN, goto that position.
;; INDENT is the current indentation level."
;;   (let ((file (rudel-find-nearest-file-line)))
;;     (speedbar-find-file-in-frame file)
;;     (save-excursion (speedbar-stealthy-updates))
;;     ;; Reset the timer with a new timeout when cliking a file
;;     ;; in case the user was navigating directories, we can cancel
;;     ;; that other timer.
;; ;    (speedbar-set-timer speedbar-update-speed)
;;     (goto-char token)
;;     (run-hooks 'speedbar-visiting-tag-hook)
;;     ;;(recenter)
;;     (speedbar-maybee-jump-to-attached-frame)
;;     ))

(provide 'rudel-speedbar)

;;; rudel-speedbar.el ends here

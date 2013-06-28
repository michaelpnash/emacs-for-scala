;;; rudel-compile.el --- Byte-compile Rudel
;;
;; Copyright (C) 2009 Phil Hagelberg
;;
;; Author: Phil Hagelberg <phil@enigma>
;; Keywords: Rudel, compile
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
;; Press M-x eval-buffer to byte-compile Rudel.


;;; History:
;;
;; 0.1 - Initial revision


;;; Code:
;;

(let ((rudel-dir (file-name-directory
		  (or (buffer-file-name) load-file-name))))
  ;; Adjust load path for compilation.
  (dolist (dir '("." "jupiter" "obby" "zeroconf"))
    (let ((d (concat rudel-dir "/" dir)))
      (add-to-list 'load-path d)))

  ;; Byte compile everything.
  (byte-recompile-directory rudel-dir 0))

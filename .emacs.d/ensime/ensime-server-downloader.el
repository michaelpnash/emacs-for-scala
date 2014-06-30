;;; ensime-server-downloader.el --- Downloads the ENSIME server

(require 'url-handlers)
(require 'xml)

;;; Code:
(defun ensime--latest-server-version (scala-version)
  "Query the remote repository for the latest ENSIME release name for scala-version."
  (let* ((ensime-server-base (concat ensime-default-server-root scala-version "/"))
	 (ensime-server-metadata (concat ensime-server-base "metadata.xml")))
    (make-directory ensime-server-base 't)
    (url-copy-file (concat ensime-server-maven-prefix scala-version "/maven-metadata.xml")
		   ensime-server-metadata
		   'OK-IF-ALREADY-EXISTS)
    (let* ((root (car (xml-parse-file ensime-server-metadata)))
	   (version (car (xml-get-children root 'version))))
      (car (xml-node-children version)))))

(defun ensime--server-jar (scala-version ensime-version)
  "Returns the locally persisted location of the (scala, ensime) versioned jar."
  (concat ensime-default-server-root scala-version "/" ensime-version ".jar"))

(defun ensime--check-server-update (scala-version)
  "Check for available updates to the ENSIME server.
   Returns string URL if available, otherwise nil."
  (let* ((latest-release (ensime--latest-server-version scala-version))
	 (latest-jar (ensime--server-jar scala-version latest-release)))
  (if (file-exists-p latest-jar) nil latest-release)))

(defun ensime--download-server (scala-version latest-release)
  "Persists the jar associated to the (scala, ensime) version pair,
   making it available for immediate use. Returns the jar name."
  (let* ((target (ensime--server-jar scala-version latest-release))
	 (url (concat ensime-server-maven-prefix scala-version "/" latest-release "/ensime_" scala-version "-" latest-release "-assembly.jar")))
    (warn "Downloading ENSIME server %s to %s (~30MB)" url target)
    (url-copy-file url target)
    target))

(defun ensime-update-server (scala-version)
  "Check for updates and download the updated server if one is available."
  (interactive "sScala version: ")
  (message "Checking for new releases of the ENSIME server for scala %s" scala-version)
  (let* ((release-version (ensime--check-server-update scala-version)))
    (when release-version
      (ensime--download-server scala-version release-version))))

;; NOTE: I tried using https://github.com/jwiegley/emacs-async/
;;       for this, but I couldn't get the load-path setup correctly
;;       for the spawned process. Doing the call to this async
;;       would be really nice.
(defun ensime-get-or-download-server (scala-version)
  "Returns the latest local jar supporting the given scala-version, or downloads one.
   Returns nil if no such jar is available locally or remotely."
  (let* ((dir (concat ensime-default-server-root scala-version))
	 (existing (if (file-exists-p dir) (directory-files-and-attributes dir t "\\.jar$") nil))
	 (latest (car (sort existing (lambda (a b) (> (float-time (sixth a)) (float-time (sixth b))))))))
    (if latest
	(progn (message
		"This ENSIME server is the latest locally available for scala %s: %s"
		scala-version (first latest))
	       (car latest))
      (warn (concat "No local ENSIME servers were found, trying to download one."
	    " This might freeze emacs if you're behind a corporate firewall."
	    " If that happens, use a developer installation (see the README)."))
      (let* ((updated (ensime-update-server scala-version)))
	(if updated updated
	  (error
	   (concat "No ENSIME servers are available for scala %s!"
		   " The remote server may be unavailable or your config is incorrect."
		   " Check ensime-default-scala-version and your project's :scala-version")
	   scala-version))))))


;(ensime-get-or-download-server "2.10")
;(ensime-get-or-download-server "2.9.3")
;(ensime-update-server "2.9.3")

(provide 'ensime-server-downloader)
;;; ensime-server-downloader.el ends here

;; Local Variables:
;; no-byte-compile: t
;; End:


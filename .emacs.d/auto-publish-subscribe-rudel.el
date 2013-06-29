(defcustom rudel-auto-publish-exclude-regexp
  (rx
   (or (group "*" (0+ anything) "*")
       " SPEEDBAR"))
  "Buffer matching this regular expression are not auto-published.

This option only has an effect when `rudel-auto-publish-predicate' is
set to `rudel-auto-publish-not-excluded-p' "
  :group 'rudel
  :type  'string)

(defcustom rudel-auto-publish-predicate
  #'rudel-auto-publish-not-excluded-p
  "This function decides whether to auto-publish buffers."
  :group 'rudel
  :type  '(choice (const    :tag "Exclude well-known unsuitable buffers"
                            rudel-auto-publish-not-excluded-p)
                  (function :tag "Other function")))

(defcustom rudel-auto-subscribe-predicate
  #'rudel-auto-subscribe-not-excluded-p
  "This function decides whether to auto-subscribe to documents."
  :group 'rudel
  :type  '(choice (const    :tag "Exclude well-known unsuitable documents"
                            rudel-auto-subscribe-not-excluded-p)
                  (function :tag "Other function")))



;;; Auto choose mode minor mode
;;

(defun rudel-auto-adjust-mode (document buffer)
  "Automatically choose an appropriate major mode for BUFFER.
Note: The idea of let-binding `buffer-file-name' is taken from
http://stackoverflow.com/questions/2375473/"
  (with-current-buffer buffer
    (if (hack-local-variables t)
        (hack-local-variables)
      (let ((buffer-file-name (buffer-name)))
        (set-auto-mode)))))

;;;###autoload
(define-minor-mode global-rudel-auto-choose-mode-minor-mode
  "Toggle the global Rudel auto choose mode minor mode.
When this mode is enabled, Rudel will try to set the mode of
created buffers when subscribing to documents.

With argument ARG positive, turn on the mode. Negative, turn off
the mode. nil means to toggle the mode."
  :init-value nil
  :global     t
  :group      'rudel
  :lighter    " +M"
  (cond

   ;; Mode is being enabled.
   (global-rudel-auto-choose-mode-minor-mode
    (add-hook 'rudel-document-attach-hook
              #'rudel-auto-adjust-mode))

   ;; Mode is being disabled.
   (t
    (remove-hook 'rudel-document-attach-hook
                 #'rudel-auto-adjust-mode)))
  )


;;; Auto publish minor mode
;;

(defun rudel-auto-publish-not-exluded-p (buffer)
  "Nil when BUFFER should be excluded from auto-publishing.
This ensures:
+ BUFFER's name does not look like `rudel-auto-publish-exclude-regexp'
+ there is no document named like BUFFER
+ BUFFER does not have an associated document"
  (and (not (string-match-p rudel-auto-publish-exclude-regexp
                            (buffer-name buffer)))
       (not (rudel-find-document
             rudel-current-session (buffer-name buffer)))
       (not (with-current-buffer buffer
              rudel-buffer-document))))

(defun rudel-auto-maybe-publish-buffer (&optional buffer)
  "Publish BUFFER if it satisfies `rudel-auto-publish-predicate'.
If BUFFER is nil, use the current buffer."
  (when (not buffer)
    (setq buffer (current-buffer)))
  (when (and rudel-current-session
             (funcall rudel-auto-publish-predicate buffer))
    (rudel-publish-buffer buffer)))

(defun rudel-publish-all-buffers ()
  "Publish all buffer satisfying `rudel-auto-publish-predicate'."
  (interactive)
  (mapc #'rudel-auto-maybe-publish-buffer (buffer-list)))

;;;###autoload
(define-minor-mode global-rudel-auto-publish-minor-mode
  "Toggle the global Rudel auto publish minor mode.
When this mode is enabled, Rudel will automatically publish all
buffers that satisfy the value of `rudel-auto-publish-predicate'.

With argument ARG positive, turn on the mode. Negative, turn off
the mode. nil means to toggle the mode."
  :init-value nil
  :global     t
  :group      'rudel
  :lighter    " +P"
  (cond

   ;; Mode is being enabled.
   (global-rudel-auto-publish-minor-mode
    ;; Publish all buffers.
    (rudel-publish-all-buffers)

    ;; Publish new buffers as soon as we notice them after a command.
    ;; TODO is this too expensive?
    (add-hook 'post-command-hook
              #'rudel-auto-maybe-publish-buffer))

   ;; Mode is being disabled.
   (t
    ;; Stop looking for new buffers.
    (remove-hook 'post-command-hook
                 #'rudel-auto-maybe-publish-buffer)))
  )


;;; Auto subscribe minor mode
;;

(defun rudel-auto-subscribe-not-excluded-p (document)
  "Nil if DOCUMENT should be excluded from auto-publishing.
This ensures:
+ DOCUMENT is not attached to a buffer
+ there is no buffer named like DOCUMENT"
  (and (not (rudel-attached-p document))
       (not (get-buffer (object-name-string document)))))

(defun rudel-auto-maybe-subscribe-to-document (document)
  "Subscribe to DOCUMENT if it satisfies `rudel-auto-subscribe-predicate'."
  (when (funcall rudel-auto-subscribe-predicate document)
    (rudel-subscribe document)))

(defun rudel-subscribe-to-all-documents ()
  "Subscribe to all documents satisfying `rudel-auto-subscribe-predicate'."
  (interactive)
  (when rudel-current-session
    (mapc #'rudel-auto-maybe-subscribe-to-document
          (oref rudel-current-session :documents))))
;; TODO make reader `rudel-documents' or `rudel-session-documents'

;;;###autoload
(define-minor-mode global-rudel-auto-subscribe-minor-mode
  "Toggle the global Rudel auto subscribe minor mode.
When this mode is enabled, Rudel will automatically subscribe to
all newly published documents.

With argument ARG positive, turn on the mode. Negative, turn off
the mode. nil means to toggle the mode."
  :init-value nil
  :global     t
  :group      'rudel
  :lighter    " +S"
  (cond

   ;; Mode is being enabled.
   (global-rudel-auto-subscribe-minor-mode
    ;; TODO handle case when there is no session (yet)
    ;; Subscribe to all available documents.
    (rudel-subscribe-to-all-documents)

    ;; Monitor the session for new documents. Subscribe when they
    ;; appear.
    (add-hook 'rudel-session-add-document-hook
              (lambda (session document)
                (rudel-auto-maybe-subscribe-to-document document))))

   ;; Mode is being disabled.
   (t
    ;; Stop looking for new documents.
    (remove-hook 'rudel-session-add-document-hook
                 (lambda (session document)
                   (rudel-auto-maybe-subscribe-to-document document)))))
  )

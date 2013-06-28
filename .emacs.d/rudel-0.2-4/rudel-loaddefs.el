;;; rudel-loaddefs.el

(autoload 'rudel-join-session "rudel" "Start a collaborative Rudel session" t)
(autoload 'rudel-host-session "rudel" "Host a collaborative Rudel session" t)
(autoload 'rudel-speedbar "rudel-speedbar"
  "Show connected users and documents for the Rudel session in speedbar" t)
(autoload 'global-rudel-minor-mode "rudel-mode"
  "Bindings for rudel session-level commands" t)

(global-set-key (kbd "C-c c j") 'rudel-join-session)

(setq rudel-dir (file-name-directory (or (buffer-file-name) load-file-name)))

(dolist (dir '("." "jupiter" "obby" "zeroconf"))
  (add-to-list 'load-path (concat rudel-dir "/" dir)))

(eval-after-load 'rudel
  '(progn
     (require 'rudel-obby)
     ;; Carefully try to load zeroconf support
     (when (and (featurep 'dbusbind)
		(require 'dbus nil t)
		(require 'zeroconf nil t)
		(dbus-get-name-owner :system "org.freedesktop.Avahi"))
       (require 'rudel-zeroconf))
     ;; Enable global minor mode
     (global-rudel-minor-mode 1)))

(provide 'rudel-loaddefs)
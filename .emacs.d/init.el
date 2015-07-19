(x-focus-frame nil)
(setq mac-command-modifier 'super)

(require 'package)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.org/packages/") t)
(when (< emacs-major-version 24)
  ;; For important compatibility libraries like cl-lib
  (add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/")))

;; List the package we want
(setq package-list '(ensime magit multiple-cursors move-text find-file-in-repository dired-details ace-jump-mode color-theme color-theme-solarized yasnippet window-numbering expand-region neotree monokai-theme tidy rainbow-delimiters key-chord markdown-mode slime))

(package-initialize) 

;; Fetch list of packages available
(unless package-archive-contents
	(package-refresh-contents))

;; install the packages that are missing, if any
(dolist (package package-list)
	(unless (package-installed-p package)
		(package-install package)))

(require 'ensime)
;; Start ensime mode whenever we open scala mode, e.g. open a .scala file
(add-hook 'scala-mode-hook 'ensime-scala-mode-hook)
;; Start ensime with Super-e
(global-set-key (kbd "C-c C-c c") 'ensime)

;; Don't show the magit instructions every time
(setq magit-last-seen-setup-instructions "1.4.0")

(require 'multiple-cursors)
(require 'multiple-cursors)
(global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)

(require 'move-text)
(move-text-default-bindings)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes (quote ("fc5fcb6f1f1c1bc01305694c59a1a861b008c534cae8d0e48e4d5e81ad718bc6" default)))
 '(tool-bar-mode nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(setq ensime-sem-high-faces
  '(
  (var . (:foreground "#ff2222"))
    (val . (:foreground "#dddddd"))
    (varField . (:foreground "#ff3333"))
    (valField . (:foreground "#dddddd"))
    (functionCall . (:foreground "#84BEE3"))
    (param . (:foreground "#ffffff"))
    (class . font-lock-type-face)
    (trait . (:foreground "#084EA8"))
    (object . (:foreground "#026DF7"))
    (package . font-lock-preprocessor-face)
))

(set-face-attribute 'default nil :height 160)

(require 'find-file-in-repository)

(global-linum-mode 1)
(global-set-key [f7] 'kill-whole-line)
(global-set-key (kbd "M-s M-m") 'magit-status)
(global-set-key (kbd "M-s M-/") 'comment-or-uncomment-region)
(global-set-key (kbd "C-x C-M-f") 'find-file-in-repository)
(global-set-key (kbd "C-c C-r r") 'ensime-refactor-rename)
(global-set-key (kbd "C-c C-o i") 'ensime-refactor-organize-imports)
(global-set-key (kbd "C-c C-i l") 'ensime-refactor-inline-local)
(global-set-key (kbd "C-c C-t i") 'ensime-inspect-by-path)
(put 'dired-find-alternate-file 'disabled nil)

;;Uncomment the below to hide the menu
;;(menu-bar-mode -1)

;; Uncomment below if you want to disable compile on save
;; (setq ensime-sbt-compile-on-save nil)

;; Some general-purpose key bindings
(global-set-key [kp-subtract] 'undo) ; [Undo]
(global-set-key (kbd "S-z") 'undo)
(global-set-key [insert]    'overwrite-mode) ; [Ins]
(global-set-key [kp-insert] 'overwrite-mode) ; [Ins]
(global-set-key (kbd "C-l") 'goto-line) ; [Ctrl]-l]
(global-set-key (kbd "C-L") 'recenter-top-bottom)
(global-set-key [f2] 'split-window-vertically)
(global-set-key [f1] 'remove-split)

(setq ido-enable-flex-matching t)
(setq ido-everywhere t)
(ido-mode 1)
(setq ido-use-filename-at-point 'guess)
(setq ido-create-new-buffer 'always)
(setq ido-file-extensions-order '(".scala" ".org" ".txt" ".py" ".emacs" ".xml" ".el" ".ini" ".cfg" ".cnf"))

(require 'dired-details)
(dired-details-install)

(require 'ace-jump-mode)
(define-key global-map (kbd "C-c SPC") 'ace-jump-mode)
(autoload
  'ace-jump-mode
  "ace-jump-mode"
  "Emacs quick move minor mode"
 t)
(autoload
  'ace-jump-mode-pop-mark
  "ace-jump-mode"
  "Ace jump back:-)"
 t)

(define-key global-map (kbd "RET") 'newline-and-indent)

(defun search-to-brace ()
  "Jump to the next open brace"
  (interactive)
  (search-forward "{"))
(define-key global-map (kbd "M-s {") 'search-to-brace)

(defun search-to-prev-brace ()
    "Jump to the previous brace"
    (interactive)
    (search-backward "{"))
(define-key global-map (kbd "M-S {") 'search-to-prev-brace)

(defun search-to-close-brace ()
  "Jump to the next close brace"
  (interactive)
  (search-forward "}"))
(define-key global-map (kbd "M-s }") 'search-to-close-brace)

(defun search-to-prev-close-brace ()
  "Jump to the previous close brace"
  (interactive)
  (search-backward "}"))
(define-key global-map (kbd "M-S }") 'search-to-prev-brace)

(defun search-to-next-def ()
  "Jump to the next def"
  (interactive)
  (search-forward "def "))
(define-key global-map (kbd "M-s d") 'search-to-next-def)

(defun search-to-prev-def ()
  "Jump to the previous def"
  (interactive)
  (search-backward "def "))
(define-key global-map (kbd "M-S d") 'search-to-prev-def)

(setq-default indent-tabs-mode nil)
(setq-default tab-width 2)
(setq indent-line-function 'insert-tab)

(define-key global-map (kbd "<backtab>") 'scala-indent:indent-with-reluctant-strategy)

;; Solarized theme
(require 'color-theme)
(require 'color-theme-solarized)
(load-theme 'solarized t)
;; Use M-x customize-variable frame-background-mode to change
(setq frame-background-mode 'dark)
(color-theme-solarized)

;; Un-comment below lines for Monokai theme
;; (require 'monokai-theme)
;; (load-theme 'monokai t)

;; Always pick up the most recent file from the filesystem
(global-auto-revert-mode 1)

(global-set-key (kbd "s-j") 'dired-jump)

(setq server-socket-dir "~/.emacs.d/server")

;; window command shortcuts
(global-set-key (kbd "s-|") 'split-window-horizontally)
(global-set-key (kbd "C-x 9") 'split-window-horizontally)
(global-set-key (kbd "s--") 'split-window-vertically)
(global-set-key (kbd "s-+") 'remove-split)
(global-set-key (kbd "s-<up>") 'enlarge-window)
(global-set-key (kbd "s-<down>") 'shrink-window)
(global-set-key (kbd "s-<right>") 'enlarge-window-horizontally)
(global-set-key (kbd "s-<left>") 'shrink-window-horizontally)
(global-set-key (kbd "M-k") 'kill-whole-line)

(global-set-key (kbd "s-1") 'ace-jump-line-mode)
(global-set-key (kbd "s-2") 'ace-jump-word-mode)

(global-set-key (kbd "s-f") 'find-file-in-repository)

;; Put temporary and backup files elsewhere
(setq auto-save-file-name-transforms
          `((".*" ,(concat user-emacs-directory "auto-save/") t))) 
(setq backup-directory-alist
      `(("." . ,(expand-file-name
                 (concat user-emacs-directory "backups")))))
(setq create-lockfiles nil)

(require 'yasnippet)
(yas-global-mode 1)
       
(require 'window-numbering)
(window-numbering-mode 1)

(require 'expand-region)
(global-set-key (kbd "C-=") 'er/expand-region)

(defun save-all () (interactive) (save-some-buffers t))
(global-set-key (kbd "S-s") 'save-all)

(global-set-key (kbd "s-R") 'ensime-inf-eval-buffer)
(global-set-key (kbd "s-r") 'ensime-inf-eval-region)

(require 'neotree)
(global-set-key (kbd "s-d") 'neotree)
(neotree)

(require 'whitespace)
(setq whitespace-line-column 120)
(setq whitespace-style '(face lines-tail))
(global-whitespace-mode +1)

(setq show-paren-delay 0)
(show-paren-mode 1)

(defun bf-pretty-print-xml-region (begin end)
  "Pretty format XML markup in region. You need to have nxml-mode
http://www.emacswiki.org/cgi-bin/wiki/NxmlMode installed to do
this.  The function inserts linebreaks to separate tags that have
nothing but whitespace between them.  It then indents the markup
by using nxml's indentation rules."
  (interactive "r")
  (save-excursion
      (nxml-mode)
      (goto-char begin)
      (while (search-forward-regexp "\>[ \\t]*\<" nil t) 
        (backward-char) (insert "\n"))
      (indent-region begin end))
    (message "Ah, much better!"))

(require 'rainbow-delimiters)
(add-hook 'scala-mode-hook #'rainbow-delimiters-mode)
(add-hook 'emacs-lisp-mode-hook #'rainbow-delimiters-mode)

;; Save on focus-out
(defun save-all ()
  (interactive)
  (save-some-buffers t))
(add-hook 'focus-out-hook 'save-all)

(require 'key-chord)
(key-chord-mode 1)
;;(key-chord-define-global "jx" 'smex)

(set-frame-font "Source Code Pro-18" nil t)

;; Exit emacs w/o prompts
(require 'cl)
(defadvice save-buffers-kill-emacs (around no-query-kill-emacs activate)
           (flet ((process-list ())) ad-do-it))

(add-hook 'markdown-mode-hook (lambda () (electric-indent-local-mode -1)))

;; Control and mouse-wheel-up to increase font size in buffer, down to decrease    
(global-set-key [C-wheel-up] '(lambda () (interactive) (text-scale-increase 1)))
(global-set-key [C-wheel-down] '(lambda () (interactive) (text-scale-decrease 1)))

;; Super wheel-up to increase font size in all buffers
(defun change-font-height (delta)
  (set-face-attribute 'default 
                      (selected-frame)
                      :height (+ (face-attribute 'default :height) delta)))
(global-set-key [M-wheel-up] '(lambda () (interactive) (change-font-height +4)))
(global-set-key [M-wheel-down] '(lambda () (interactive) (change-font-height -4)))

(setq inferior-lisp-program "/usr/local/bin/sbcl")



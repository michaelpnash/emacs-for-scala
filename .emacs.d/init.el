(add-to-list 'load-path "~/.emacs.d/scala-mode2")
(require 'scala-mode)
;; Load the ensime lisp code...
(add-to-list 'load-path "~/.emacs.d/ensime/ensime_2.10.0-RC3-0.9.8.2/elisp")
(require 'ensime)

(add-to-list 'load-path "~/.emacs.d/sunrise-commander")
(require 'sunrise-commander)
(require 'sunrise-x-tree)

;; This step causes the ensime-mode to be started whenever
;; scala-mode is started for a buffer. You may have to customize this step
;; if you're not using the standard scala mode.
(add-hook 'scala-mode-hook 'ensime-scala-mode-hook)

;; MINI HOWTO: 
;; Open .scala file. M-x ensime (once per project)

(custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
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

(add-to-list 'load-path "~/emacs/color-theme-6.6.0/")
(require 'color-theme)
(eval-after-load "color-theme"
  '(progn
       (color-theme-initialize)
            (color-theme-hober)))

(set-face-attribute 'default nil :height 160)

(global-linum-mode 1)
(global-set-key (kbd "s-o") 'find-grep-dired)
(global-set-key (kbd "s-d") 'sunrise)
(global-set-key (kbd "s-/") 'comment-or-uncomment-region)
(global-set-key (kbd "s-O") 'find-name-dired)
(global-set-key (kbd "C-c C-r r") 'ensime-refactor-rename)
(global-set-key (kbd "C-c C-o i") 'ensime-refactor-organize-imports)
(global-set-key (kbd "C-c C-i l") 'ensime-refactor-inline-local)
(global-set-key (kbd "C-c C-t i") 'ensime-inspect-by-path)
(put 'dired-find-alternate-file 'disabled nil)
(menu-bar-mode -1)
(setq ensime-sbt-compile-on-save nil)

(global-set-key (kbd "s-N") 'scala-find-name)
(global-set-key (kbd "s-n") 'scala-find-class)

(setq project-dir (getenv "PWD"))

(defun scala-find-name ()
  "Find-name-dired in current directory"
  (interactive)
  (find-name-dired (format "%s/src" project-dir) (format "%s%s" (read-from-minibuffer "Scala File:") ".scala")))   

(defun scala-find-class ()
  "Find-name-grep in current directory for class trait or object"
  (interactive)
  (setq name (read-from-minibuffer "Object/Trait/Class:"))
  (find-grep-dired (format "%s/src" project-dir) (format "class %s" name))
  )


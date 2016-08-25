(require 'package)

(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/"))
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/"))
(add-to-list 'package-archives
             '("gnu" . "http://elpa.gnu.org/packages/"))
(package-initialize)

;; Load Use Package and Dep. ;;

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))
(require 'diminish)
(require 'bind-key)

;; Load all of my personal lisp files
(add-to-list 'load-path "~/.emacs.d/lisp/")

;;; Package Settings ;;;
(use-package ac)
(use-package ac-html
  :ensure t)

(use-package ac-html-bootstrap
  :ensure t)

(use-package ac-js2
  :ensure t)

(use-package coffee-mode
  :ensure t)

(use-package emmet-mode
  :ensure t
  :bind (("C-=" . er/expand-region)))

(use-package eww
  :bind ("C-c e" . eww)
  :init (setq browse-url-browser-function 'eww-browse-url))

(use-package expand-region
  :ensure t)

(use-package git-gutter
  :ensure t)

(use-package gruvbox-theme
  :ensure t)

(use-package helm
  :ensure t)

(use-package helm-ag
  :ensure t)

(use-package helm-dash
  :ensure t)

(use-package helm-projectile
  :ensure t
  :init
  (projectile-global-mode)
  (setq projectile-completion-system 'helm)
  (helm-projectile-on))

(use-package ido-ubiquitous
  :ensure t)

(use-package ido-vertical-mode
  :ensure t)

(use-package js2-mode
  :ensure t)

(use-package less-css-mode
  :ensure t)

(use-package magit
  :ensure t
  :bind
  ("C-x g" . magit-status)
  ("C-x M-g" . magit-dispatch-popup))

(use-package multiple-cursors
  :ensure t
  :bind
  ("C->" . mc/mark-next-like-this)
  ("C-<" . mc/mark-previous-like-this)
  ("C-c C-<" . mc/mark-all-like-this))

(use-package nodejs-repl
  :ensure t)

(use-package projectile
  :ensure t
  :config
  (projectile-global-mode)
  :init
  (setq projectile-indexing-method 'alien))

;; (use-package tern
;;   :init
;;   (add-to-list 'load-path "~/Repositories/tern/emacs")
;;   (autoload 'tern-mode "tern.el" nil t)
;;   :config
;;   (add-hook 'js2-mode (lambda () (tern-mode t)))

(use-package web-beautify
  :ensure t)

(use-package web-mode
  :ensure t)

(use-package yaml-mode
  :ensure t)

;;; Emacs Settings ;;;

;; Standard settings ;;
(if (file-exists-p "~/.emacs.d/settings/editor.el")
    (load-file "~/.emacs.d/settings/editor.el"))

;; Custom settings - not in repo ;;
(if (file-exists-p "~/.emacs.d/settings/custom-editor.el")
    (load-file "~/.emacs.d/settings/custom-editor.el"))

;; Markdown Mode
(autoload 'markdown-mode "markdown-mode"
  "Major mode for editing Markdown files" t)

(setq markdown-command "/usr/local/bin/markdown")
(setq markdown-css-dir "~/.emacs.d/custom files/markdown-mode/")
(setq markdown-css-theme "github")


;; Ansi-Term Settings
(add-hook 'term-mode-hook (lambda ()
                            (define-key term-raw-map (kbd "C-y") 'term-paste)))

(defun set-exec-path-from-shell-PATH ()
  (let ((path-from-shell (shell-command-to-string "$SHELL -c 'echo $PATH'")))
    (setenv "PATH" path-from-shell)
    (setq exec-path (split-string path-from-shell path-separator))))
(when window-system (set-exec-path-from-shell-PATH))
(setenv "PATH" (concat (getenv "PATH") ":/usr/local/bin"))
(setq exec-path (append exec-path '("/usr/local/bin")))

;; Helm Settings
(require 'helm)
(require 'helm-config)

;; The default "C-x c" is quite close to "C-x C-c", which quits Emacs.
;; Changed to "C-c h". Note: We must set "C-c h" globally, because we
;; cannot change `helm-command-prefix-key' once `helm-config' is loaded.
(global-set-key (kbd "C-c h") 'helm-command-prefix)
(global-unset-key (kbd "C-x c"))
(global-set-key (kbd "C-x C-f") 'helm-find-files)
(global-set-key (kbd "C-x b") 'helm-buffers-list)
;; Use Helm for emacs commands
(global-set-key (kbd "M-x") 'helm-M-x)

(define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action)

(helm-mode 1)

(eval-after-load 'js2-mode
  '(define-key js2-mode-map (kbd "C-c b") 'web-beautify-js))
(eval-after-load 'json-mode
  '(define-key json-mode-map (kbd "C-c b") 'web-beautify-js))
(eval-after-load 'web-mode
  '(define-key web-mode-map (kbd "C-c b") 'web-beautify-html))
(eval-after-load 'css-mode
  '(define-key css-mode-map (kbd "C-c b") 'web-beautify-css))


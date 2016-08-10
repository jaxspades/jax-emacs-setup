(require 'package)

(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/"))
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/"))

(package-initialize)

;; Load all of my personal lisp files
(add-to-list 'load-path "~/.emacs.d/lisp/")

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

;; Projectile Settings
(projectile-global-mode)
(setq projectile-indexing-method 'alien)

;; Web Beautify Settings
(eval-after-load 'js2-mode
  '(define-key js2-mode-map (kbd "C-c b") 'web-beautify-js))
(eval-after-load 'json-mode
  '(define-key json-mode-map (kbd "C-c b") 'web-beautify-js))
(eval-after-load 'web-mode
  '(define-key web-mode-map (kbd "C-c b") 'web-beautify-html))
(eval-after-load 'css-mode
  '(define-key css-mode-map (kbd "C-c b") 'web-beautify-css))

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

;; Helm Projectile
(projectile-global-mode)
(setq projectile-completion-system 'helm)
(helm-projectile-on)

;; Expand Region
(require 'expand-region)
(global-set-key (kbd "C-=") 'er/expand-region)

;; Multiple Cursors
(require 'multiple-cursors)
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)

;; Magit Settings
(global-set-key (kbd "C-x g") 'magit-status)
(global-set-key (kbd "C-x M-g") 'magit-dispatch-popup)

;; EWW Settings
(global-set-key (kbd "C-c e") 'eww)
(setq browse-url-browser-function 'eww-browse-url)

;; Add some automatic modes based on file extentions
(add-to-list 'auto-mode-alist '("\\.py\\'" . python-mode))
(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
(add-to-list 'auto-mode-alist '("\\.html\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.cfc\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.css\\'" . css-mode))
(add-to-list 'auto-mode-alist '("\\.less\\'" . less-css-mode))
(add-to-list 'auto-mode-alist '("\\.scss\\'" . less-css-mode))
(add-to-list 'auto-mode-alist '("\\.cfm\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.org\\'" . org-mode))
(add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))


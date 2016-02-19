(package-initialize)
;;; Cask Package Management ;;;

;; Define my list of desired packages
(require 'cask "~/.cask/cask.el")
(cask-initialize)

;; Load all of my personal lisp files
(add-to-list 'load-path "~/.emacs.d/lisp/")

;;; Emacs Look and Feel ;;;

;; Use a sweet font
(add-to-list 'default-frame-alist '(font . "ProFontWindows-16"))
;; Use a sweet theme
(load "gruvbox-theme")
;; (load "monokai-theme")
;; (load "spacegray-theme")

;; Turn off suto-save, the visible-bell and the startup message
(setq backup-inhibited 't
      auto-save-default 'nil
      visible-bell 'nil
      inhibit-startup-message 't
      inhibit-startup-echo-area-message '"")

;; Get rid of the distracting toolbar and scrollbar
(tool-bar-mode -1)
(toggle-scroll-bar -1)
;; Add some line numbers
(global-linum-mode 1)

;; Add the time and file path to the mode line
(display-time-mode 1)

;; Display the file path for the buffer name
;; TODO: Somehow only show from the point of the project root using projectile, if in a project...
;; TODO: Replace my home path with ~
;;(setq-default mode-line-buffer-identification
;;            (cons (car mode-line-buffer-identification) '(buffer-file-name)))

;; How about some config settings?
(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)

;; Add a bit more space and a vertical line to my line number section
(setq linum-format "%4d \u2502 ")

;;; Text Settings ;;;

;; Turn off tabs.
(setq-default indent-tabs-mode nil)

;; Delete Regions
(delete-selection-mode 1)

;;; Package Settings ;;;

;; Turn on auto complete for code and for some various commands with ac and ido
(ac-config-default)

;; Turn on ido for wherever Helm isn't completing stuff.
(ido-mode 1)
(ido-everywhere 1)

;; disable ido faces to see flx highlights.
(setq ido-enable-flex-matching t)
(setq ido-use-faces nil)

;; Make ido vertical.
(ido-vertical-mode 1)
(setq ido-vertical-define-keys 'C-n-and-C-p-only)

;; Make sure we have LF endings
(setq-default buffer-file-coding-system 'utf-8-unix)

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
(add-to-list 'auto-mode-alist '("\\.cfm\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.org\\'" . org-mode))
(add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))

(if (file-exists-p "~/.emacs.d/custom-init.el")
  (load-file "~/.emacs.d/custom-init.el"))


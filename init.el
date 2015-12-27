(package-initialize)
;;; Cask Package Management ;;;

;; Define my list of desired packages
(require 'cask "~/.cask/cask.el")
(cask-initialize)

;;; Emacs Look and Feel ;;;

;; Use a sweet font
(add-to-list 'default-frame-alist '(font . "ProFontWindows-16"))
;; Use a sweet theme
(load "monokai-theme")

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
;; Add the time to the mode line
(display-time-mode 1)

;; How about some config settings?
(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)

;; Add a bit more space and a vertical line to my line number section
(setq linum-format "%4d \u2502 ")

;;; Text Settings ;;;

;; Turn off tabs.
(setq-default indent-tabs-mode nil)

;;; Package Settings ;;;

;; Load Smex, which gives auto-completion within an M-x command
(autoload 'smex "smex")
(global-set-key (kbd "M-x") 'smex)

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

;; Add some automatic modes based on file extentions
(add-to-list 'auto-mode-alist '("\\.py\\'" . python-mode))
(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
(add-to-list 'auto-mode-alist '("\\.html\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.cfc\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.css\\'" . css-mode))
(add-to-list 'auto-mode-alist '("\\.less\\'" . less-css-mode))
(add-to-list 'auto-mode-alist '("\\.cfm\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.org\\'" . org-mode))

;; Projectile Settings
(projectile-global-mode)
(global-set-key (kbd "C-c f") 'projectile-find-file)
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

(define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action)

(helm-mode 1)

;; Helm Projectile
(projectile-global-mode)
(setq projectile-completion-system 'helm)
(helm-projectile-on)

;; Load all of my personal lisp files
(add-to-list 'load-path "~/.emacs.d/lisp/")

(if (file-exists-p "~/.emacs.d/custom-init.el")
  (load-file "~/.emacs.d/custom-init.el"))


;; Use a sweet font
(add-to-list 'default-frame-alist '(font . "ProFontWindows-16"))
;; Use a sweet theme
(load "gruvbox-theme")

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

;; highlight the current line
;; but only if it is not the char terminal
(add-hook 'after-change-major-mode-hook
          '(lambda () (hl-line-mode (if (equal major-mode 'term-mode) 0 1))))

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


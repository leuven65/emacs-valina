;;; -*- lexical-binding: t -*-

;; (load-theme 'modus-vivendi-deuteranopia)
(load-theme 'doom-dracula t)

;; (setq startup-screen-inhibit-startup-screen t)
(setq inhibit-startup-screen t)
(setq inhibit-splash-screen t)
(setq inhibit-startup-echo-area-message t)
(setq ring-bell-function 'ignore)
(setq use-short-answers t)
(setq fill-column 120)

(keymap-global-set "C-x C-k"
		   (lambda ()
		     (interactive)
		     (kill-buffer (current-buffer))))

;; (fido-vertical-mode +1)
(delete-selection-mode +1)
(electric-pair-mode +1)
(global-display-line-numbers-mode +1)
;; (global-display-fill-column-indicator-mode +1)

(setq package-archives
      '(("elpa" . "https://elpa.gnu.org/packages/")
        ("elpa-devel" . "https://elpa.gnu.org/devel/")
        ("nongnu" . "https://elpa.nongnu.org/nongnu/")
        ("melpa" . "https://melpa.org/packages/")))

(use-package doom-themes
  )

(use-package doom-modeline
  :hook (after-init . doom-modeline-mode)
  )

(use-package helm
  :hook (after-init . helm-mode)
  :bind (("M-x" . helm-M-x)
         ("C-x C-f" . helm-find-files)
	 ("C-x b" . helm-mini)
	 ("C-x C-b" . helm-mini)
	 ("C-h a" . helm-apropos)
	 ("M-i" . helm-occur)
         )
  :custom
  (helm-move-to-line-cycle-in-source nil)
  (helm-follow-input-idle-delay 0)
  (helm-x-icons-provider 'nerd-icons)
  (helm-buffers-show-icons t)
  (helm-bookmark-use-icon t)
  :config
  (helm-ff-icon-mode +1)
  )

(use-package helm-descbinds
  :hook (after-init . helm-descbinds-mode)
  )

(use-package company
  :hook (after-init . global-company-mode)
  :custom
  (company-idle-delay 0)
  (company-minimum-prefix-length 2)
  :config
  (company-tng-mode +1)
  )

(use-package undo-fu
  :bind (("C-/" . undo-fu-only-undo)
	 ("C-?". undo-fu-only-redo)
	 )
  :custom
  (undo-fu-ignore-keyboard-quit nil)
  )

(use-package ace-window
  :bind (("M-o" . ace-window))
  )

(use-package system-packages
  :custom
  (system-packages-use-sudo nil)
  )

(use-package symbol-overlay
  :bind (("C-=" . symbol-overlay-put)
	 ("C-+" . symbol-overlay-remove-all))
  )

(use-package copilot
  :custom
  (copilot-log-max nil)
  (copilot-server-log-level 4)
  )

(use-package diff-hl
  :hook ((after-init . global-diff-hl-mode)
	 (after-init . diff-hl-flydiff-mode))
  :custom
  (vc-git-program (substitute-in-file-name "${SCOOP}/apps/git/current/mingw64/bin/git.exe"))
  (vc-allow-async-revert t)
  (vc-allow-async-diff t)
  (vc-async-checkin t)
  (vc-display-status nil) ; used by `vc-mode-line'
  
  (diff-hl-update-async t)
  )

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("5c7720c63b729140ed88cf35413f36c728ab7c70f8cd8422d9ee1cedeb618de5"
     default))
 '(package-selected-packages
   '(ace-window casual-symbol-overlay company copilot copilot-chat
		diff-hl doom-modeline doom-themes helm helm-descbinds
		pdf-tools symbol-overlay system-packages undo-fu)))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "Cascadia Code SemiLight" :foundry "outline" :slant normal :weight light :height 113 :width normal)))))

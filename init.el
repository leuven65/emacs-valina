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

(setopt scroll-conservatively 101)
(setopt scroll-preserve-screen-position t)
;; (setopt scroll-margin 3)

(setopt register-use-preview 'insist)


(keymap-global-set "C-x C-k"
		   (lambda ()
		     (interactive)
		     (kill-buffer (current-buffer))))

;; (fido-vertical-mode +1)
(delete-selection-mode +1)
(electric-pair-mode +1)
(global-display-line-numbers-mode +1)
;; (global-display-fill-column-indicator-mode +1)
(pixel-scroll-precision-mode)
(repeat-mode)

(add-hook 'help-fns-describe-function-functions
          #'shortdoc-help-fns-examples-function)

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

  (helm-apropos-show-short-doc t)

  (helm-grep-input-idle-delay 0.5)
  
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
  :hook (prog-mode . copilot-mode)
  :bind (:map copilot-completion-map
	      ("<tab>" . 'copilot-accept-completion))
  :custom
  (copilot-indent-offset-warning-disable t)
  ;; (copilot-log-max nil)
  ;; (copilot-server-log-level 4)
  (copilot-server-executable
   (car (directory-files-recursively
	 (substitute-env-in-file-name "${SCOOP}/apps/msys2/current/ucrt64/bin/node_modules/@github/copilot-language-server")
	 "copilot-language-server.exe")))
  )

(use-package vc
  :custom
  (vc-git-program (substitute-in-file-name "${SCOOP}/apps/git/current/mingw64/bin/git.exe"))
  (vc-allow-async-revert t)
  (vc-allow-async-diff t)
  (vc-async-checkin t)
  (vc-display-status nil) ; used by `vc-mode-line'
  :config
  (remove-hook 'find-file-hook #'vc-refresh-state)
  )

(use-package diff-hl
  :vc (:url "https://github.com/leuven65/diff-hl.git"
	    :branch "test-aio"
	    :rev :newest)
  :hook ((after-init . global-diff-hl-mode)
	 (after-init . diff-hl-flydiff-mode))
  :custom
  (diff-hl-update-async t)
  (diff-hl-show-staged-changes nil)
  (diff-hl-highlight-reference-function nil)
  :config
  ;; (define-advice diff-hl-update (:before-while (&rest _) my-advice)
  ;;   (get-buffer-window (current-buffer) t))
  )

(use-package vterm
  :vc (:url "https://github.com/xhcoding/emacs-libvterm.git"
	    :branch "master"
	    :rev :newest)
  :bind (:map vterm-mode-map
	 ( "C-q" . vterm-send-next-key))
  :custom
  ;; (vterm-decode-coding-system locale-coding-system)
  (vterm-kill-buffer-on-exit t)
  :commands (my-vterm-git-bash)
  :config
  (defun my-vterm-git-bash ()
    (interactive)
    (let ((vterm-shell "c:/Users/User/scoop/apps/git/current/usr/bin/bash.exe")
	  (vterm-environment '("VTERM_CONTEXT=GIT_BASH"))
	  (vterm-buffer-name "*vterm-git-bash*")
	  )
      (vterm--internal #'pop-to-buffer-same-window t)
      (setq-local kill-buffer-query-functions nil)
      )
    )
  ;; (split-string-shell-command (format "conhost.exe --headless --width %s --height %s -- %s" width height (vterm--get-shell)))
  )

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("5c7720c63b729140ed88cf35413f36c728ab7c70f8cd8422d9ee1cedeb618de5"
     default))
 '(package-selected-packages nil)
 '(package-vc-selected-packages
   '((diff-hl :url "https://github.com/leuven65/diff-hl.git" :branch
	      "test-on-callback-instead-of-thread")
     (vterm :url "https://github.com/xhcoding/emacs-libvterm.git"
	    :branch "master"))))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "Cascadia Code SemiLight" :foundry "outline" :slant normal :weight light :height 113 :width normal))))
 '(wgrep-delete-face ((t (:background "#282a36" :foreground "#ff5555" :bold t))))
 '(wgrep-done-face ((t (:background "#282a36" :foreground "#50fa7b"))))
 '(wgrep-error-face ((t (:background "#282a36" :foreground "#ff5555" :underline t))))
 '(wgrep-face ((t (:background "#44475a" :foreground "#f8f8f2")))))

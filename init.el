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
(setq scroll-step 1)

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

(setopt use-package-always-defer t)

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

(use-package copilot-chat
  :bind (:map copilot-chat-prompt-mode-map
	      ("M-p" . my/copilot-chat-select-prompt)
	      :map polymode-minor-mode-map
	      ("M-n" . nil)
	      )
  :config
  (defun my/copilot-chat-select-prompt ()
    "Select a prompt from the current Copilot chat history."
    (interactive)
    (when-let* ((instance (copilot-chat--current-instance))
		(prompt (completing-read "Select PROMPT: "
					 (seq-map
					  (lambda (msg)
					    (plist-get msg :content))
					  (seq-filter
					   (lambda (msg)
					     (equal (plist-get msg :role) "user"))
					   (copilot-chat-history instance))))))
      (copilot-chat--insert-prompt instance prompt)))
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
	    :branch "master"
	    :rev :newest)
  :hook ((after-init . global-diff-hl-mode)
	 (after-init . diff-hl-flydiff-mode))
  :custom
  (diff-hl-update-async t)
  (diff-hl-show-staged-changes nil)
  (diff-hl-highlight-reference-function nil)
  (diff-hl-show-hunk-function 'diff-hl-show-hunk-inline-popup)
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

(use-package gt
  :defer   :custom
  (gt-tts-speaker "mpg123")
  :hook (gt-buffer-render-init . my-gt-buffer-render-setup)
  :config
  ;; (setopt gt-http-backend (pdd-curl-backend))

  (setq gt-langs '(zh en nl))
  (setq gt-default-translator
	(gt-translator
	 :taker   (gt-taker :text 'word :pick nil :prompt t)
	 :engines (list
		   ;; (gt-google-engine :parse (gt-google-summary-parser))
		   (gt-google-engine :parse (gt-google-parser))
		   (gt-bing-engine))
	 :render  (gt-buffer-render)))

  (defun my-gt-buffer-render-setup ()
    (visual-line-mode 1)
    (visual-wrap-prefix-mode 1)
    (setq-local cursor-type 'box)
    )
  )


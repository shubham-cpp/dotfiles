;;; init.el --- -*- lexical-binding: t -*-
;;  Author: Shubham Pawar
;;; Commentary:
;;  This is my personal Emacs configuration
;;  Copied from https://github.com/ianyepan/.wsl-emacs.d/blob/master/init.el
;;; Code:

;;; Commentary: Personal config with all
;;; Code:

(defun efs/display-startup-time ()
  "Show the startup time."
  (message "Emacs loaded in %s with %d garbage collections."
	   (format "%.2f seconds"
		   (float-time
		    (time-subtract after-init-time before-init-time)))
	   gcs-done))

(add-hook 'emacs-startup-hook #'efs/display-startup-time)

(setq user-emacs-directory "~/.cache/emacs-my")
(setq package-user-dir (expand-file-name (concat user-emacs-directory "/elpa")))

(require 'package)
(setq package-archives
      '(("melpa" . "https://melpa.org/packages/")
        ("elpa" . "https://elpa.gnu.org/packages/")
        ("nongnu" . "https://elpa.nongnu.org/nongnu/")))
(package-initialize)
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-and-compile
  (setq confirm-kill-processes nil
	create-lockfiles nil
	use-package-always-ensure t
	use-package-expand-minimally t
	read-file-name-completion-ignore-case t
	read-buffer-completion-ignore-case t
	completion-ignore-case t
	global-auto-revert-non-file-buffers t
	display-line-numbers-type 'relative
	indent-tabs-mode nil
	browse-url-browser-function 'xwidget-webkit-browse-url
	frame-resize-pixelwise t
	completion-styles '(partial-completion
			    substring
			    initials
			    flex)
	completion-category-overrides '((file
					 (styles
					  partial-completion)))))
(fset 'yes-or-no-p 'y-or-n-p)
(global-auto-revert-mode +1)
(global-display-line-numbers-mode t)
;; (eval-when-compile (require 'use-package))

;; Fonts
(set-face-attribute 'default nil :font "JetBrainsMono Nerd Font" :height 105)
;; Set the fixed pitch face
(set-face-attribute 'fixed-pitch nil :font "JetBrainsMono Nerd Font" :height 105)
;; Set the variable pitch face
(set-face-attribute 'variable-pitch nil :font "JetBrainsMono Nerd Font" :height 105 :weight 'regular)

;; Default packages
(use-package cus-edit
  :ensure nil
  :config
  (setq custom-file (concat user-emacs-directory "customs.el")))

(use-package savehist
  :ensure nil
  :hook (vertico-mode . savehist-mode))

(use-package saveplace
  :ensure nil
  :hook (after-init . save-place-mode)
  :custom (save-place-limit 100))

(use-package flyspell
  :ensure nil
  :init
  (setq ispell-program-name "aspell")
  :hook ((markdown-mode org-mode) . flyspell-mode))

(use-package recentf
  :ensure nil
  :custom
  (recentf-save-file "~/.cache/recentf")
  (recentf-max-menu-items 25)
  :bind
  ([remap recentf-open-file] . consult-recent-file)
  :config
  (recentf-mode))

(use-package uniquify
  :ensure nil
  :custom
  (uniquify-buffer-name-style 'forward))

(use-package mwheel
  :ensure nil
  :config (setq mouse-wheel-scroll-amount '(2 ((shift) . 1))
		mouse-wheel-progressive-speed nil))
(use-package paren
  :ensure nil
  :after evil-collection
  :custom
  (show-paren-delay 0.1)
  (show-paren-highlight-openparen t)
  (show-paren-when-point-inside-paren t)
  (show-paren-when-point-in-periphery t)
  :config (show-paren-mode 1))

(use-package elec-pair
  :ensure nil
  :hook ((prog-mode emacs-lisp-mode org-mode) . electric-pair-mode))

;; (use-package hideshow
;;   :ensure nil
;;   :hook ((prog-mode emacs-lisp-mode) . hs-minor-mode)
;;   :config
;;   (eval-and-compile
;;     (setq hs-allow-nesting t)))

;; Dired
(use-package dired
  :ensure nil
  :commands (dired dired-jump)
  :init
  (setq trash-directory (expand-file-name (format "%s/.cache/trash-emacs" (getenv "HOME")))
	delete-by-moving-to-trash t)
  :custom
  (dired-listing-switches "-Aghov --group-directories-first")
  (dired-deletion-confirmer #'y-or-n-p))

(use-package dired-single
  :commands (dired dired-jump))

(use-package all-the-icons-dired
  :hook (dired-mode . all-the-icons-dired-mode))

(use-package dired-git
  :hook (dired-mode . dired-git-mode))

;; Theming
(use-package doom-themes
  :custom
  (doom-themes-org-fontify-special-tag t)
  (doom-gruvbox-dark-variant "hard")
  :custom-face
  (region                         ((t (:extend nil))))
  (font-lock-comment-face         ((t (:italic t))))
  (sml/modified                   ((t (:foreground "white" :bold t))))
  (hl-todo                        ((t (:inverse-video nil :italic t :bold t))))
  (highlight-symbol-face          ((t (:background "#355266" :distant-foreground "#bbbbbb"))))
  ;;(show-paren-match               ((t (:foreground "#eeeeee" :background "#444444" :bold t))))
  (highlight                      ((t (:foreground "#4db2ff" :background nil :underline t)))) ; link hover
  (link                           ((t (:foreground "#3794ff"))))
  (evil-ex-substitute-replacement ((t (:strike-through nil))))
  (vertical-border                ((t (:foreground "black" :background "black"))))
  (fringe                         ((t (:background nil))))
  :config
  (doom-themes-org-config)
  (load-theme 'doom-one t))

(use-package all-the-icons)

(use-package doom-modeline
  :hook (after-init . doom-modeline-mode)
  :custom
  (doom-modeline-height 15)
  (doom-modeline-enable-word-count t)
  (doom-modeline-buffer-encoding nil))

;; Tabs
(use-package centaur-tabs
  :demand
  :init
  (tab-bar-mode -1)
  :custom
  ((centaur-tabs-set-icons t)
   (centaur-tabs-gray-out-icons 'buffer)
   (centaur-tabs-set-modified-marker t)
   (centaur-tabs-modified-marker "•"))
  :bind (("<C-next>" . centaur-tabs-forward)
	 ("<C-prior>" . centaur-tabs-backward))
  :config
  (centaur-tabs-mode t))

;; Evil
(use-package evil
  :init
  (setq evil-want-C-u-scroll t
	evil-split-window-below t
	evil-vsplit-window-below t
	evil-want-keybinding nil
	evil-undo-system 'undo-fu
	evil-want-Y-yank-to-eol t
	evil-kill-on-visual-paste nil
	evil-shift-width 2)
  :hook (after-init . evil-mode)
  :preface
  (defun my/escape-and-nohl ()
    (interactive)
    (keyboard-escape-quit)
    (evil-ex-nohighlight))
  (defun ian/save-and-kill-this-buffer ()
    (interactive)
    (save-buffer)
    (kill-this-buffer))
  (defun my/evil-shift-right ()
    (interactive)
    (evil-shift-right evil-visual-beginning evil-visual-end)
    (evil-normal-state)
    (evil-visual-restore))

  (defun my/evil-shift-left ()
    (interactive)
    (evil-shift-left evil-visual-beginning evil-visual-end)
    (evil-normal-state)
    (evil-visual-restore))
  :config
  (with-eval-after-load 'evil-maps ; avoid conflict with company tooltip selection
    (define-key evil-insert-state-map (kbd "C-n") nil)
    (define-key evil-insert-state-map (kbd "C-p") nil))
  (evil-define-key 'visual global-map (kbd ">") #'my/evil-shift-right)
  (evil-define-key 'visual global-map (kbd "<") #'my/evil-shift-left)
  (define-key evil-insert-state-map (kbd "C-S-v") #'yank)
  (define-key evil-normal-state-map (kbd ",w") #'save-buffer)
  (define-key evil-normal-state-map (kbd "0") #'evil-first-non-blank)
  (global-set-key (kbd "<escape>") #'my/escape-and-nohl)
  (evil-ex-define-cmd "q" #'kill-this-buffer)
  (evil-ex-define-cmd "wq" #'ian/save-and-kill-this-buffer)
  (evil-global-set-key 'motion "j" #'evil-next-visual-line)
  (evil-global-set-key 'motion "k" #'evil-previous-visual-line))

(use-package evil-collection
  :after evil
  :config
  (setq evil-collection-company-use-tng nil)
  (evil-collection-init))

(use-package general
  :after evil-collection
  :preface
  (defun jib/split-window-vertically-and-switch ()
    (interactive)
    (split-window-vertically)
    (other-window 1))
  (defun jib/split-window-horizontally-and-switch ()
    (interactive)
    (split-window-horizontally)
    (other-window 1))
  (defun jib/toggle-maximize-buffer ()
    "Maximize buffer"
    (interactive)
    (if (and (= 1 (length (window-list)))
             (assoc ?_ register-alist))
	(jump-to-register ?_)
      (progn
	(window-configuration-to-register ?_)
	(delete-other-windows))))
  :config
  (general-create-definer sp/leader-keys
    :keymaps 'override
    :states  '(insert emacs normal visual)
    :prefix "SPC"
    :global-prefix "M-SPC")

  (sp/leader-keys
    "." '(find-file :which-key "find file")
    "," '(consult-recent-file :which-key "recent files")
    "/" '(evil-commentary-line :which-key "toggle comment")
    "f" '(nil :which-key "files")
    "fb" '(consult-bookmark :which-key "bookmarks")
    "ff" '(find-file :which-key "find file")
    "ff" '(find-file-other-window :which-key "find file(other)")
    "fd" '(dired-jump :which-key "open dired")
    "fr" '(consult-recent-file :which-key "recent files")
    "fR" '(rename-file :which-key "rename file")
    "fs" '(save-buffer :which-key "save buffer")
    "fS" '(evil-write-all :which-key "save all buffers")
    "fp" '(affe-find :which-key "Fzf"))
  ;; Buffer
  (sp/leader-keys
    "b" '(nil :which-key "buffer")
    "bb" '(consult-buffer :which-key "switch buffers")
    "bd" '(evil-delete-buffer :which-key "delete buffer")
    "bi" '(clone-indirect-buffer  :which-key "indirect buffer")
    "br" '(revert-buffer :which-key "revert buffer")
    "bn" '(next-buffer :which-key "switch to next buffer")
    "bp" '(previous-buffer :which-key "switch to prev buffer")
    "bk" '(kill-this-buffer :which-key "kill current buffer")
    "bK" '(kill-buffer :which-key "kill buffer"))
  (sp/leader-keys
    "w" '(nil :which-key "window")
    "wm" '(jib/toggle-maximize-buffer :which-key "maximize buffer")
    "ws" '(jib/split-window-vertically-and-switch :which-key "split below")
    "wv" '(jib/split-window-horizontally-and-switch :which-key "split right")
    "wN" '(make-frame :which-key "make frame")
    "wd" '(evil-window-delete :which-key "delete window")
    "wl" '(evil-window-right :which-key "evil-window-right")
    "wh" '(evil-window-left :which-key "evil-window-left")
    "wj" '(evil-window-down :which-key "evil-window-down")
    "wk" '(evil-window-up :which-key "evil-window-up")
    "wz" '(text-scale-adjust :which-key "text zoom")
    "wo" 'delete-other-windows
    "ww" 'evil-window-next
    "wq" '(evil-window-delete :which-key "delete window")
    "w+" 'balance-windows
    "w-" #'((lambda()
	      (interactive)
	      (evil-window-decrease-width 10)) :which-key "Win size--")
    "w=" #'((lambda()
	      (interactive)
	      (evil-window-decrease-width -10)) :which-key "Win size++"))
  (sp/leader-keys
    "e"  '(:ignore t :which-key "Eval")
    "ed" '(eval-defun :which-key "function")
    "ee" '(eval-last-sexp :which-key "current expr")
    "ex" '(eval-expression :which-key "whole expression"))
  (sp/leader-keys
    "h" '(nil :which-key "help/emacs")
    "hv" '(describe-variable :which-key "des. variable")
    "hb" '(describe-bindings :which-key "des. bindings")
    "hM" '(describe-mode :which-key "des. mode")
    "hf" '(describe-function :which-key "des. func")
    "hF" '(describe-face :which-key "des. face")
    "hk" '(describe-key :which-key "des. key")
    "hm" '(nil :which-key "switch mode")
    "hme" '(emacs-lisp-mode :which-key "elisp mode")
    "hmo" '(org-mode :which-key "org mode")
    "hmt" '(text-mode :which-key "text mode"))
  (sp/leader-keys
    "o"   '(:ignore t :which-key "org mode")
    "oi"  '(:ignore t :which-key "insert")
    "oil" '(org-insert-link :which-key "insert link")
    "on"  '(org-toggle-narrow-to-subtree :which-key "toggle narrow")
    "oa"  '(org-agenda :which-key "status")
    "ot"  '(org-todo-list :which-key "todos")
    "oc"  '(org-capture t :which-key "capture")
    "ox"  '(org-export-dispatch t :which-key "export"))
  (sp/leader-keys
    "t" '(nil :which-key "toggles")
    "tT" '(toggle-truncate-lines :which-key "truncate lines")
    "tv" '(visual-line-mode :which-key "visual line mode")
    "ta" '(mixed-pitch-mode :which-key "variable pitch mode")
    "tc" '(visual-fill-column-mode :which-key "visual fill column mode")
    "te" '(eshell-toggle :which-key "eshell")
    "tv" '(vterm-other-window :which-key "vterm(other)")
    "tt" '(load-theme :which-key "load theme")
    "tR" '(read-only-mode :which-key "read only mode")
    "tr" '(display-fill-column-indicator-mode :which-key "fill column indicator")
    "tm" '(hide-mode-line-mode :which-key "hide modeline mode"))
  (general-define-key
   :keymaps 'dired-mode-map
   :states 'normal
   "h" 'dired-single-up-directory
   "H" 'dired-omit-mode
   "l" 'dired-single-buffer
   ;; "y" 'dired-ranger-copy
   ;; "X" 'dired-ranger-move
   ;; "p" 'dired-ranger-paste
   ))

(use-package evil-surround
  :after evil-collection
  :config
  (global-evil-surround-mode 1))

(use-package evil-replace-with-register
  :after evil-collection
  :config
  (setq evil-replace-with-register-key (kbd "x"))
  (evil-replace-with-register-install)
  (evil-global-set-key 'normal (kbd "X") (kbd "x $")))

(use-package undo-fu
  :bind (:map evil-normal-state-map
              ("u" . undo-fu-only-undo)
              ("C-r" . undo-fu-only-redo)))

(use-package undo-fu-session
  :after undo-fu
  :init
  (global-undo-fu-session-mode))

(use-package evil-quickscope
  :after evil-surround
  :config
  (global-evil-quickscope-mode 1))

(use-package evil-matchit
  :hook ((web-mode
          html-mode
          mhtml-mode
          js-mode
          typescript-mode
          ) . turn-on-evil-matchit-mode))

(use-package evil-numbers
  :bind ((:map evil-visual-state-map
               ("g =" . evil-numbers/inc-at-pt)
               ("g -" . evil-numbers/dec-at-pt))
	 (:map evil-normal-state-map
               ("g =" . evil-numbers/inc-at-pt)
               ("g -" . evil-numbers/dec-at-pt))))

(use-package evil-goggles
  :after evil-collection
  :config
  (evil-goggles-mode)
  (evil-goggles-use-diff-faces))

(use-package evil-commentary
  :after evil-goggles
  :bind ("C-/" . evil-commentary-line)
  :diminish
  :config (evil-commentary-mode +1))

(use-package evil-args
  :bind ((:map evil-inner-text-objects-map
               ("a" . evil-inner-arg))
	 (:map evil-outer-text-objects-map
               ("a" . evil-outer-arg))
	 (:map evil-normal-state-map
               ("L" . evil-forward-arg)
               ("H" . evil-backward-arg)
               ("K" . evil-jump-out-args))
	 (:map evil-motion-state-map
               ("L" . evil-forward-arg)
               ("H" . evil-backward-arg))))

;; Selection Frameworks

(use-package affe
  :commands (affe-grep affe-find)
  :custom
  (affe-find-command "rg --files --hidden --ignore-file .gitignore")
  :config
  ;; Configure Orderless
  ;;  (setq affe-regexp-function #'orderless-pattern-compiler
  ;;    affe-highlight-function #'orderless--highlight)

  ;; Manual preview key for `affe-grep'
  (consult-customize affe-grep :preview-key (kbd "M-.")))

;; (use-package orderless
;;   :after marginalia
;;   :custom
;;   (orderless-matching-styles '(orderless-initialism
;;                 orderless-literal
;;                 orderless-regexp))
;;   (completion-styles '(orderless))
;;   (completion-category-defaults nil)
;;   (completion-category-overrides '((file (styles
;;                    partial-completion)))))

(use-package vertico
  :after which-key
  :init
  (setq completion-in-region-function
	(lambda (&rest args)
	  (apply (if vertico-mode
		     #'consult-completion-in-region
		   #'completion--in-region)
		 args)))
  (setq read-buffer-completion-ignore-case t)
  ;; (setq vertico-scroll-margin 10)
  (setq vertico-resize t)
  (setq vertico-cycle t)
  (vertico-mode)
  :preface
  (defun sp/minibuffer-backward-kill (arg)
    "When minibuffer is completing a file name delete up to parent
    folder, otherwise delete a character backward"
    (interactive "p")
    (if minibuffer-completing-file-name
	(if (string-match-p "/." (minibuffer-contents))
            (zap-up-to-char (- arg) ?/)
	  (delete-minibuffer-contents))
      (delete-backward-char arg)))
  :bind (:map vertico-map
              ("?" . minibuffer-completion-help)
              ("C-j" . vertico-next)
              ("C-k" . vertico-previous)
              ("C-d" . vertico-scroll-down)
              ("C-u" . vertico-scroll-up)
              ("C-<backspace>" . sp/minibuffer-backward-kill)))

(use-package consult
  ;; :hook (completion-list-mode . consult-preview-at-point-mode)
  :bind
  (("M-y" . consult-yank-pop)
   ("C-s" . consult-line)
   :map isearch-mode-map
   ("M-e" . consult-isearch-history)))

(use-package marginalia
  :init
  (marginalia-mode))

(use-package embark
  :bind
  (("C-." . embark-act)
   ("C-;" . embark-dwim)
   ;; ("C-h B" . embark-bindings)
   ([remap describe-bindings] . embark-bindings))
  :init
  (setq prefix-help-command #'embark-prefix-help-command))

(use-package embark-consult
  :after (embark consult)
  :demand t
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

;; Git
(use-package git-gutter
  :hook ((prog-mode org-mode) . git-gutter-mode)
  :config
  (setq git-gutter:update-interval 2)
  ;; These characters are used in terminal mode
  ;; (setq git-gutter:modified-sign "≡")
  ;; (setq git-gutter:added-sign "≡")
  ;; (setq git-gutter:deleted-sign "≡")
  (set-face-foreground 'git-gutter:added "LightGreen")
  (set-face-foreground 'git-gutter:modified "LightGoldenrod")
  (set-face-foreground 'git-gutter:deleted "LightCoral"))
(use-package git-gutter-fringe
  :after git-gutter
  :config
  (define-fringe-bitmap 'git-gutter-fr:added [224] nil nil '(center repeated))
  (define-fringe-bitmap 'git-gutter-fr:modified [224] nil nil '(center repeated))
  (define-fringe-bitmap 'git-gutter-fr:deleted [128 192 224 240] nil nil 'bottom))

(use-package magit
  :commands magit-status
  :custom
  (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1)
  :bind
  ("C-c g" . magit-status))

;; Linting

(use-package flycheck
  :hook (prog-mode . flycheck-mode)
  :preface
  (defun my/typescript-mode-setup ()
    "Custom setup for Typescript mode"
    (setq flycheck-checker #'javascript-eslint))
  :config
  (add-hook 'typescript-mode-hook #'my/typescript-mode-setup)
  (general-define-key
   :states 'normal
   :keymaps 'flycheck-mode-map
   "] g" '(flycheck-next-error :which-key "Next error")
   "[ g" '(flycheck-previous-error :which-key "Prev error")))

(use-package flycheck-pos-tip
  :hook (flycheck-mode . flycheck-pos-tip-mode))

;; Code Completion
(use-package company
  :hook ((prog-mode emacs-lisp-mode) . company-mode)
  :preface
  (defun my/company-backtab ()
    (interactive)
    (company-complete-common-or-cycle -1))
  :custom
  (company-show-quick-access t)
  (company-idle-delay 0.0)
  (company-tooltip-minimum-width 60)
  (company-tooltip-maximum-width 60)
  (company-tooltip-limit 12)
  (company-minimum-prefix-length 1)
  (company-selection-wrap-around t)
  (company-tooltip-align-annotations t)
  (company-frontends '(company-pseudo-tooltip-frontend
                       company-echo-metadata-frontend))
  :bind
  (:map company-active-map
	("C-j" . company-select-next-or-abort)
	("C-k" . company-select-previous-or-abort)
	("C-w"    . backward-kill-word)
	("C-n" . company-select-next)
	("C-p" . company-select-previous)
	("C-s"  . company-search-candidates)
	("C-c"    . company-search-abort)
	("TAB" . company-select-next-or-abort)
	([tab] . company-select-next-or-abort)
	(([backtab] . company-select-previous-or-abort))))

(use-package company-box
  :hook (company-mode . company-box-mode))

(use-package company-quickhelp
  :hook (company-mode . company-quickhelp-mode))

;; Code Formattings
(use-package format-all
  :preface
  (defun ian/format-code ()
    "Auto-format whole buffer."
    (interactive)
    (let ((windowstart (window-start)))
      (if (derived-mode-p 'prolog-mode)
          (prolog-indent-buffer)
        (format-all-buffer))
      (set-window-start (selected-window) windowstart))
    (message "Format-all completed"))
  (defalias 'format-document #'ian/format-code)
  :hook ((prog-mode emacs-lisp-mode) . format-all-mode)
  :bind ([F6] . ian/format-code)
  :config
  (add-hook 'prog-mode-hook #'format-all-ensure-formatter)
  (add-hook 'python-mode-hook #'(lambda ()
                                  (setq-local format-all-formatters '(("Python" yapf))))))
;; Enhance defaults

(use-package which-key
  :after centaur-tabs
  :diminish which-key-mode
  :custom
  (which-key-idle-delay 1)
  :config
  (which-key-mode))

(use-package helpful
  :commands (helpful-callable helpful-variable helpful-command helpful-key)
  :bind
  ([remap describe-function] . helpful-function)
  ([remap describe-command] . helpful-command)
  ([remap describe-variable] . helpful-variable)
  ([remap describe-key] . helpful-key))

(use-package async
  :defer t
  :init
  (dired-async-mode 1)
  (async-bytecomp-package-mode 1)
  :custom (async-bytecomp-allowed-packages '(all)))


(use-package no-littering)

(eval-and-compile
  (setq auto-save-file-name-transforms
	`((".*" ,(no-littering-expand-var-file-name "auto-save/") t))))

;; Extra Highlights

(use-package rainbow-delimiters
  :hook ((prog-mode emacs-lisp-mode) . rainbow-delimiters-mode))

(use-package rainbow-mode
  :hook ((rjsx-mode web-mode scss-mode) . rainbow-mode)
  :bind* ("C-c r" . rainbow-mode))

(use-package highlight-indent-guides
  :custom
  (highlight-indent-guides-method 'column)
  :hook ((prog-mode emacs-lisp-mode) . highlight-indent-guides-mode))

(use-package highlight-quoted
  :hook (emacs-lisp-mode . highlight-quoted-mode))

(use-package hl-todo
  :hook ((prog-mode elisp-lisp-mode) . rainbow-mode)
  :config
  (add-to-list 'hl-todo-keyword-faces '("DOING" . "#94bff3"))
  (add-to-list 'hl-todo-keyword-faces '("WHY" . "#7cb8bb")))

(use-package centered-cursor-mode
  :hook ((prog-mode emacs-lisp-mode org-mode) . centered-cursor-mode))

;; Org-mode
(use-package org
  :hook ((org-mode . visual-line-mode)
         (org-mode . auto-fill-mode)
         (org-mode . org-indent-mode)
         (org-mode . (lambda ()
                       (setq-local evil-auto-indent nil))))
  :custom
  (org-link-descriptive nil)
  (org-startup-folded nil)
  (org-todo-keywords '((sequence "TODO" "DOING" "DONE")))
  :config
  (require 'org-tempo)
  (add-to-list 'org-structure-template-alist '("sh" . "src sh"))
  (add-to-list 'org-structure-template-alist '("el" . "src emacs-lisp"))
  (add-to-list 'org-structure-template-alist '("sc" . "src scheme"))
  (add-to-list 'org-structure-template-alist '("ts" . "src typescript"))
  (add-to-list 'org-structure-template-alist '("py" . "src python"))
  (add-to-list 'org-structure-template-alist '("go" . "src go"))
  (add-to-list 'org-structure-template-alist '("yaml" . "src yaml"))
  (add-to-list 'org-structure-template-alist '("json" . "src json"))
  (add-to-list 'org-file-apps '("\\.pdf\\'" . emacs))
  (setq org-html-checkbox-type 'html))

(use-package org-bullets
  :hook (org-mode . org-bullets-mode)
  :custom
  (org-bullets-bullet-list '("◉" "○" "●" "○" "●" "○" "●")))

(use-package org-roam
  :init
  (setq org-roam-v2-ack t)
  :custom
  (org-roam-directory "~/Documents/Notes/Roam")
  (org-roam-completion-everywhere t)
  :bind (("C-c n l" . org-roam-buffer-toggle)
	 ("C-c n f" . org-roam-node-find)
	 ("C-c n i" . org-roam-node-insert)
	 :map org-mode-map
	 ("C-M-i"    . completion-at-point))
  :commands (org-roam-buffer-toggle org-roam-node-insert org-roam-node-find)
  :config
  (org-roam-setup))

;; Terminal & Eshell

(use-package vterm
  :commands vterm
  :custom
  (vterm-shell "zsh")
  (vterm-max-scrollback 10000))

(use-package vterm-toggle
  :after vterm
  :config
  (global-set-key [f2] 'vterm-toggle)
  (global-set-key [C-f2] 'vterm-toggle-cd)
  (define-key vterm-mode-map [f2] 'vterm-toggle))

(defun efs/configure-eshell ()
  "Add hooks and other settings for eshell."
  (add-hook 'eshell-pre-command-hook 'eshell-save-some-history)
  (add-to-list 'eshell-output-filter-functions 'eshell-truncate-buffer)
  (evil-normalize-keymaps)

  (eval-and-compile
    (setq eshell-history-size  10000
	  eshell-buffer-maximum-lines 10000
	  eshell-hist-ignoredups t
	  eshell-scroll-to-bottom-on-input t)))

(use-package eshell-git-prompt
  :after eshell)

(use-package eshell
  :hook (eshell-first-time-mode . efs/configure-eshell)
  :config

  (with-eval-after-load 'esh-opt
    (setq eshell-destroy-buffer-when-process-dies t)
    (setq eshell-visual-commands '("htop" "zsh" "nvim")))

  (eshell-git-prompt-use-theme 'powerline))

(use-package esh-autosuggest
  :hook (eshell-mode . esh-autosuggest-mode)
  :config
  (setq esh-autosuggest-delay 0.5)
  (set-face-foreground 'company-preview-common "#4b5668")
  (set-face-background 'company-preview nil))

(use-package eshell-did-you-mean
  :after eshell-syntax-highlighting
  :config
  (eshell-did-you-mean-setup))

(use-package eshell-toggle
  :after eshell
  :custom
  (eshell-toggle-size-fraction 3)
  (eshell-toggle-run-command nil)
  :bind
  ("C-`" . eshell-toggle))

(use-package fish-completion
  :hook (eshell-mode . fish-completion-mode))

(use-package eshell-syntax-highlighting
  :after esh-mode
  :config
  (eshell-syntax-highlighting-global-mode +1))

;; LSP
(use-package lsp-mode
  :commands (lsp lsp-deferred)
  :hook (prog-mode . lsp-mode)
  :init
  (setq lsp-keymap-prefix "C-l"
	lsp-completion-provider :none
	lsp-clients-clangd-args "--header-insertion-decorators=0 --background-index -pch-storage=memory --clang-tidy --suggest-missing-includes --cross-file-rename --completion-style=detailed"
	lsp-css-lint-box-model "warning"
	lsp-css-lint-duplicate-properties "warning"
	lsp-css-lint-empty-rules "error"
	lsp-css-lint-float "error"
	lsp-css-lint-zero-units "warning"
	lsp-lua-completion-call-snippet "Both"
	lsp-lua-hint-enable t
	lsp-lua-workspace-max-preload 2000
	lsp-lua-workspace-preload-file-size 1000
	lsp-lua-diagnostics-disable ["lowercase-global"]
	lsp-lua-diagnostics-globals ["describe"]
	lsp-lua-runtime-unicode-name t)
  :custom
  (lsp-disabled-clients '((python-mode . pyls)))
  (lsp-eslint-run "onSave")
  (lsp-eslint-auto-fix-on-save t)
  :config
  (lsp-enable-which-key-integration t)
  (general-define-key
   :states '(normal visual motion)
   :keymaps 'lsp-mode-map
   :prefix "g"
   "d" '(lsp-find-definition :which-key "Goto defination")
   "r" '(lsp-find-references :which-key "Find references")
   "a" '(lsp-ui-sideline-apply-code-actions :which-key "Code actions")
   "o" '(lsp-organize-imports :which-key "Organize Imports")
   "I" '(lsp-javascript-rename-file :which-key "JS rename file")
   "R" '(lsp-rename :which-key "Rename")))

(use-package lsp-ui
  :hook ((lsp-mode lsp-deferred) . lsp-ui-mode)
  :custom
  (lsp-ui-doc-position 'bottom)
  (lsp-ui-doc-enable t)
  (lsp-ui-doc-use-childframe t)
  (lsp-ui-doc-include-signature t)
  (lsp-ui-sideline-enable nil)
  (lsp-ui-flycheck-enable t)
  (lsp-ui-flycheck-list-position 'right)
  (lsp-ui-flycheck-live-reporting t)
  (lsp-ui-peek-enable t)
  (lsp-ui-peek-list-width 60)
  (lsp-ui-peek-peek-height 25))

(use-package yasnippet
  :hook ((lsp-deferred org-mode) . yas-minor-mode))

(use-package yasnippet-snippets
  :after yasnippet)

;; Additional modes
(use-package add-node-modules-path
  :after (rjsx-mode typescript-mode)
  :config
  ;; Enable add-node-modules-path for specific modes
  (dolist (mode '(typescript-mode-hook
		  rjsx-mode-hook
		  json-mode-hook
		  web-mode-hook))
    (add-hook mode #'add-node-modules-path)))

(use-package typescript-mode
  :mode "\\.ts\\'"
  :hook (typescript-mode . lsp-deferred)
  :config
  (setq typescript-indent-level 2))

(use-package js2-mode
  :mode "\\.js\\'"
  :hook (js2-mode . lsp-deferred))

(use-package rjsx-mode
  :mode ("\\.tsx\\'" "\\.jsx\\'")
  :hook (rjsx-mode . lsp-deferred))

(use-package jest
  :hook ((js2-mode rjsx-mode typescript-mode) . jest-minor-mode))

(use-package npm-mode
  :hook ((js2-mode rjsx-mode typescript-mode) . npm-mode))

(use-package web-mode
  :hook (web-mode . lsp-deferred)
  :mode ("\\.html?\\'" "\\.css\\'")
  :config
  (setq web-mode-enable-current-element-highlight t
	web-mode-enable-current-column-highlight t)
  (general-define-key
   :prefix ","
   :states 'motion
   :keymaps 'web-mode-map
   "i" '(web-mode-buffer-indent :which-key "web mode indent")
   "c" '(web-mode-fold-or-unfold :which-key "web mode toggle fold")
   ))

(use-package emmet-mode
  :hook ((web-mode rjsx-mode) . emmet-mode)
  :preface
  (defun my/emmet-jsx ()
    (setq-local emmet-expand-jsx-className? t))
  :config
  (setq emmet-insert-flash-time 0.001)
  (add-hook 'js-jsx-mode-hook #'my/emmet-jsx)
  (add-hook 'web-mode-hook #'my/emmet-jsx))

(use-package scss-mode
  :mode "\\.scss\\'"
  :custom
  (scss-compile-at-save nil)
  :config
  (web-mode))

(use-package json-mode
  :hook (lsp-deferred . json-mode))

(use-package yaml-mode
  :hook (lsp-deferred . yaml-mode)
  :config
  (evil-define-key 'insert yaml-mode-map "RET" #'newline-and-indent))

(use-package lsp-pyright
  :preface
  (defun my/pyright-start()
    (interactive)
    (require 'lsp-pyright)
    (lsp-deferred))
  :hook (python-mode . my/pyright-start))

(use-package lsp-tailwindcss
  :after (rjsx-mode web-mode)
  :init
  (setq lsp-tailwindcss-add-on-mode t))

(use-package grip-mode
  :commands (grip-mode))

(use-package markdown-mode
  :hook (markdown-mode . auto-fill-mode)
  :config
  (set-face-attribute 'markdown-code-face nil :inherit 'org-block))

(use-package projectile
  :hook (prog-mode . projectile-mode)
  :config
  (general-define-key
   "C-c p" '(:keymap projectile-command-map :package projectile))
  (general-define-key
   :prefix "SPC"
   :keymaps 'normal
   "p" '(:keymap projectile-command-map :wk "Project")))

;; Hacks
(defun sp/improve-word-length ()
  "This way, when do a 'e' (evil-forward-word-end) it is better.
Even playing with symbol, when inside a string, it becomes a word"
  (modify-syntax-entry ?_ "w")
  (modify-syntax-entry ?- "w"))
(add-hook 'after-change-major-mode-hook #'sp/improve-word-length)

(provide 'init)
;;; init.el ends here

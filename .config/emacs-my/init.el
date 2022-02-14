(setq gc-cons-threshold most-positive-fixnum)

(setq comp-async-report-warnings-errors nil) ;; native-comp warning
(setq byte-compile-warnings '(not free-vars unresolved noruntime lexical make-local))

(defun efs/display-startup-time ()
  (message "Emacs loaded in %s with %d garbage collections."
           (format "%.2f seconds"
                   (float-time
                    (time-subtract after-init-time before-init-time)))
           gcs-done))

(add-hook 'emacs-startup-hook #'efs/display-startup-time)

(setq user-emacs-directory "~/.cache/emacs-my")

(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;; Install use-package
(straight-use-package 'use-package)
;; (setq use-package-verbose t)
(use-package straight
  :custom (straight-use-package-by-default t))

(use-package gcmh
  :diminish gcmh-mode
  :config
  (setq gcmh-idle-delay 5
        gcmh-high-cons-threshold (* 16 1024 1024))  ; 16mb
  (gcmh-mode 1))

(set-face-attribute 'default nil :font "JetBrainsMono Nerd Font" :height 105)
;; Set the fixed pitch face
(set-face-attribute 'fixed-pitch nil :font "JetBrainsMono Nerd Font" :height 105)
;; Set the variable pitch face
(set-face-attribute 'variable-pitch nil :font "JetBrainsMono Nerd Font" :height 105 :weight 'regular)

(use-package doom-themes
  :init (load-theme 'doom-one t)
  :config
  (doom-themes-org-config))

(use-package all-the-icons)

(use-package doom-modeline
  :init (doom-modeline-mode 1)
  :custom
  (doom-modeline-height 15)
  (doom-modeline-enable-word-count t)
  (doom-modeline-buffer-encoding nil))

(use-package scroll-bar
  :straight nil
  :config (scroll-bar-mode -1))

(setq display-line-numbers-type 'relative)
(global-display-line-numbers-mode t)

(use-package mwheel
  :straight nil
  :config (setq mouse-wheel-scroll-amount '(2 ((shift) . 1))
                mouse-wheel-progressive-speed nil))

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
  (evil-define-key 'visual global-map (kbd ">") 'my/evil-shift-right)
  (evil-define-key 'visual global-map (kbd "<") 'my/evil-shift-left)
  (define-key evil-insert-state-map (kbd "C-S-v") 'yank)
  (define-key evil-normal-state-map (kbd ",w") 'save-buffer)
  (define-key evil-normal-state-map (kbd "0") 'evil-first-non-blank)
  (global-set-key (kbd "<escape>") #'(lambda()
                                      (interactive)
                                      (keyboard-escape-quit)
                                      (evil-ex-nohighlight)))
  (evil-ex-define-cmd "q" #'kill-this-buffer)
  (evil-ex-define-cmd "wq" #'ian/save-and-kill-this-buffer)
  (evil-global-set-key 'motion "j" 'evil-next-visual-line)
  (evil-global-set-key 'motion "k" 'evil-previous-visual-line))

(use-package evil-collection
  :after evil
  :config
  ;; (setq evil-collection-company-use-tng nil)
  (evil-collection-init))

(use-package general
  :after evil
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
  "tm" '(hide-mode-line-mode :which-key "hide modeline mode")))

(use-package evil-goggles
  :after evil-collection
  :config
  (evil-goggles-mode)
  (evil-goggles-use-diff-faces))

(use-package evil-commentary
  :after evil
  :diminish
  :config (evil-commentary-mode +1))

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

(use-package evil-numbers
  :bind (:map evil-normal-state-map
              ("g =" . evil-numbers/inc-at-pt)
              ("g -" . evil-numbers/dec-at-pt)))
    ;;; 1 alphanumeric 2

(use-package cus-edit
  :straight nil
  :config
  (setq custom-file (concat user-emacs-directory "to-be-dumped.el")))

(use-package emacs
  :straight nil
  :init
  (defun crm-indicator (args)
    (cons (concat "[CRM] " (car args)) (cdr args)))
  (advice-add #'completing-read-multiple :filter-args #'crm-indicator)
  (setq minibuffer-prompt-properties
        '(read-only t cursor-intangible t face minibuffer-prompt))
  (add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)
  (setq enable-recursive-minibuffers t)
  :preface
  (defvar ian/indent-width 4)
  :config
  (setq frame-title-format '("Yay-Evil")
        ring-bell-function 'ignore
        visible-bell t
        scroll-margin 12
        idle-update-delay 1.0
        browse-url-browser-function 'xwidget-webkit-browse-url
        frame-resize-pixelwise t)
  (fset 'yes-or-no-p 'y-or-n-p)
  (tool-bar-mode -1)
  (menu-bar-mode -1)
  ;; Always use spaces for indentation
  (setq-default indent-tabs-mode nil
                tab-width ian/indent-width)
  (setq inhibit-startup-screen t))

(use-package files
  :straight nil
  :config
  (setq confirm-kill-processes nil
        create-lockfiles nil))

(use-package elec-pair
  :straight nil
  :hook ((prog-mode emacs-lisp-mode org-mode) . electric-pair-mode))

(use-package whitespace
  :straight nil
  :hook (before-save . whitespace-cleanup))

(use-package paren
  :straight nil
  :custom
  (show-paren-delay 0.1)
  (show-paren-highlight-openparen t)
  (show-paren-when-point-inside-paren t)
  (show-paren-when-point-in-periphery t)
  :config (show-paren-mode 1))

(use-package autorevert
  :straight nil
  :config
  (global-auto-revert-mode +1)
  (setq auto-revert-interval 5
        auto-revert-check-vc-info t
        global-auto-revert-non-file-buffers t
        auto-revert-verbose nil))

(use-package ediff
  :straight nil
  :config
  (setq ediff-window-setup-function #'ediff-setup-windows-plain)
  (setq ediff-split-window-function #'split-window-horizontally))

(use-package minibuffer
  :straight nil
  :custom
  (read-file-name-completion-ignore-case t))

(use-package recentf
  :straight nil
  :custom
  (recentf-save-file "~/.cache/recentf")
  (recentf-max-menu-items 25)
  :bind
  ([remap recentf-open-file] . consult-recent-file)
  :config
  (recentf-mode))

(use-package uniquify
  :straight nil
  :custom
  (uniquify-buffer-name-style 'forward))

(use-package dired
  :straight nil
  :commands (dired dired-jump)
  :custom
  (dired-listing-switches "-Aghov --group-directories-first")
  (dired-deletion-confirmer #'y-or-n-p))

(general-define-key
 :keymaps 'dired-mode-map
 :states 'normal
 "h" 'dired-single-up-directory
 "H" 'dired-omit-mode
 "l" 'dired-single-buffer
 "y" 'dired-ranger-copy
 "X" 'dired-ranger-move
 "p" 'dired-ranger-paste)

(use-package dired-single
  :commands (dired dired-jump))

(use-package all-the-icons-dired
  :hook (dired-mode . all-the-icons-dired-mode))

(use-package dired-git
  :hook (dired-mode . dired-git-mode))

(use-package no-littering)
(setq auto-save-file-name-transforms
      `((".*" ,(no-littering-expand-var-file-name "auto-save/") t)))

(use-package rainbow-delimiters
  :hook ((prog-mode emacs-lisp-mode) . rainbow-delimiters-mode))

(use-package rainbow-mode
  :hook ((rjsx-mode web-mode scss-mode) . rainbow-mode))

(use-package highlight-indent-guides
  :custom
  (highlight-indent-guides-method 'column)
  :hook ((prog-mode emacs-lisp-mode) . highlight-indent-guides-mode))

(use-package highlight-quoted
  :hook (emacs-lisp-mode . highlight-quoted-mode))

(use-package hl-prog-extra
  :commands (hl-prog-extra-mode)
  :hook (prog-mode . hl-prog-extra-mode))

(use-package git-gutter
  :hook ((text-mode prog-mode org-mode) . git-gutter-mode)
  :config
  (setq git-gutter:update-interval 2)
  ;; These characters are used in terminal mode
  ;; (setq git-gutter:modified-sign "≡")
  ;; (setq git-gutter:added-sign "≡")
  ;; (setq git-gutter:deleted-sign "≡")
  (set-face-foreground 'git-gutter:added "LightGreen")
  (set-face-foreground 'git-gutter:modified "LightGoldenrod")
  (set-face-foreground 'git-gutter:deleted "LightCoral"))

(use-package which-key
  :defer 0
  :diminish which-key-mode
  :config
  (which-key-mode)
  (setq which-key-idle-delay 1))

(use-package helpful
  :commands (helpful-callable helpful-variable helpful-command helpful-key)
  :bind
  ([remap describe-function] . helpful-function)
  ([remap describe-command] . helpful-command)
  ([remap describe-variable] . helpful-variable)
  ([remap describe-key] . helpful-key))

(use-package centaur-tabs
  :demand
  :custom
  ((centaur-tabs-set-icons t)
   (centaur-tabs-gray-out-icons 'buffer)
   (centaur-tabs-set-modified-marker t)
   (centaur-tabs-modified-marker "•"))
  :bind (("<C-next>" . centaur-tabs-forward)
         ("<C-prior>" . centaur-tabs-backward))
  :config
  (centaur-tabs-mode t))

(use-package async
  :defer t
  :init
  (dired-async-mode 1)
  (async-bytecomp-package-mode 1)
  :custom (async-bytecomp-allowed-packages '(all)))

(use-package affe
  :commands (affe-grep affe-find)
  :custom
  (affe-find-command "rg --files --hidden --ignore-file .gitignore")
  :config
  ;; Configure Orderless
  (setq affe-regexp-function #'orderless-pattern-compiler
        affe-highlight-function #'orderless--highlight)

  ;; Manual preview key for `affe-grep'
  (consult-customize affe-grep :preview-key (kbd "M-.")))

(use-package orderless
  :after marginalia
  :custom
  (orderless-matching-styles '(orderless-initialism
                               orderless-literal
                               orderless-regexp))
  (completion-styles '(orderless))
  (completion-category-defaults nil)
  (completion-category-overrides '((file (styles
                                          partial-completion)))))

(use-package vertico
  :after orderless
  :init
  (setq completion-in-region-function
        (lambda (&rest args)
          (apply (if vertico-mode
                     #'consult-completion-in-region
                   #'completion--in-region)
                 args)))
  (setq read-buffer-completion-ignore-case t)
  (vertico-mode)
  (setq vertico-cycle t)
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
              ;; ("<backspace>" . sp/minibuffer-backward-kill)
              ("C-<backspace>" . sp/minibuffer-backward-kill)
              ))

(use-package savehist
  :hook (vertico-mode . savehist-mode)
  :straight nil
  ;; :init
  ;; (savehist-mode +1)
  )

(use-package saveplace
  :straight nil
  :hook (after-init . save-place-mode)
  :custom (save-place-limit 100)
  ;; :config (save-place-mode t))
  )

(use-package consult
  :hook (completion-list-mode . consult-preview-at-point-mode)
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
   ("C-h B" . embark-bindings))
  :init
  (setq prefix-help-command #'embark-prefix-help-command))

(use-package embark-consult
  :after (embark consult)
  :demand t
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

(use-package magit
  :commands magit-status
  :custom
  (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1))
(general-define-key
 :states '(normal motion visual)
 :keymaps 'override
 :prefix "SPC"
 ;; :global-prefix "C-SPC"
 "g" '(nil :which-key "Git")
 "gg" '(magit-status :which-key "Open magit"))

(use-package avy
  :bind (:map evil-normal-state-map
              ("s" . evil-avy-goto-char-2)
              ("S" . evil-avy-goto-word-0)))

(use-package vterm
  :commands vterm
  :config
  (setq vterm-shell "zsh")
  (setq vterm-max-scrollback 10000))
(use-package vterm-toggle
  :after vterm
  :config
  (global-set-key [f2] 'vterm-toggle)
  (global-set-key [C-f2] 'vterm-toggle-cd)
  (define-key vterm-mode-map [f2] 'vterm-toggle))

(defun efs/configure-eshell ()
  (add-hook 'eshell-pre-command-hook 'eshell-save-some-history)
  (add-to-list 'eshell-output-filter-functions 'eshell-truncate-buffer)
  (evil-normalize-keymaps)

  (setq eshell-history-size  10000
        eshell-buffer-maximum-lines 10000
        eshell-hist-ignoredups t
        eshell-scroll-to-bottom-on-input t))

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
  (eshell-toggle-run-command nil))

(use-package fish-completion
  :hook (eshell-mode . fish-completion-mode))

(use-package eshell-syntax-highlighting
  :after esh-mode
  :config
  (eshell-syntax-highlighting-global-mode +1))

(defun sp/improve-word-length ()
  "This way, when do a 'e' (evil-forward-word-end) it is better.
Even playing with symbol, when inside a string, it becomes a word"
  (modify-syntax-entry ?_ "w")
  (modify-syntax-entry ?- "w"))
(add-hook 'after-change-major-mode-hook #'sp/improve-word-length)

(defun sp/move-line-up ()
  (interactive)
  (sp/save-column
   (transpose-lines 1)
   (forward-line -2)))

(defun sp/move-line-down ()
  (interactive)
  (sp/save-column
   (forward-line 1)
   (transpose-lines 1)
   (forward-line -1)))

(global-set-key (kbd "M-<up>") 'sp/move-line-up)
(global-set-key (kbd "M-<down>") 'sp/move-line-down)

(use-package company
  :defer t
  :bind (:map company-active-map
              ("<tab>"  . company-indent-or-complete-common)
              ([tab]  . company-indent-or-complete-common)
              ("<backtab>" . (lambda() (interactive) (company-complete-common-or-cycle -1)))
              ("C-n"    . company-select-next)
              ("C-p"    . company-select-previous)
              ("C-j" . company-select-next-or-abort)
              ("C-k" . company-select-previous-or-abort)
              ("C-w"    . backward-kill-word)
              ("C-g"    . company-abort)
              ("C-c"    . company-search-abort)
              ("C-s"  . company-search-candidates)
              ("C-l" . company-other-backend)
              ("C-o" . company-search-toggle-filtering))
  :hook ((prog-mode org-mode) . company-mode)
  :custom
  (company-minimum-prefix-length 2)
  (company-idle-delay 0.2)
  (company-show-quick-access t)
  (selection-coding-system 'utf-8)
  (company-dabbrev-ignore-case t)
  (company-dabbrev-other-buffers t)
  (company-dabbrev-code-ignore-case t)
  (company-dabbrev-code-other-buffers nil)
  (company-dabbrev-minimum-length 3)
  (company-tooltip-limit 20)
  (company-lsp-enable-snippet t)
  (company-transformers '(company-sort-by-occurrence))
  (company-backends
   '((company-capf :with company-yasnippet)
     (company-dabbrev :with company-yasnippet)
     (company-files :with company-yasnippet)
     (company-dabbrev-code
      company-keywords
      :with company-yasnippet)
     )))
;; (dolist (mode '(
;;                 ;;prog-mode
;;                 emacs-lisp-mode))
;;   (add-hook mode (lambda ()
;;                    (setq completion-styles '(partial-completion)))))

(use-package company-quickhelp
  :after company
  :config
  (company-quickhelp-mode 1))

(use-package lsp-mode
  :commands (lsp lsp-deferred)
  :hook (prog-mode . lsp-mode)
  :init
  (setq lsp-keymap-prefix "C-l"
        lsp-completion-provider :none)
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
   "R" '(lsp-rename :which-key "Rename")
   ))

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
  :hook ((lsp-deferred org-mode) . yas-minor-mode)
  ;; :config
  ;; (yas-global-mode 1)
  )

(use-package yasnippet-snippets
  :after yasnippet)

(use-package flycheck
  :hook (prog-mode . flycheck-mode)
  :preface
  (defun typescript-mode-setup ()
    "Custom setup for Typescript mode"
    (setq flycheck-checker 'javascript-eslint))
  :config
  (add-hook 'typescript-mode-hook 'typescript-mode-setup)
  (general-define-key
   :states 'normal
   :keymaps 'flycheck-mode-map
   "] g" '(flycheck-next-error :which-key "Next error")
   "[ g" '(flycheck-previous-error :which-key "Prev error")
   ))

(use-package flycheck-pos-tip
  :hook (flycheck-mode . flycheck-pos-tip-mode))

(use-package add-node-modules-path
  :after (rjsx-mode typescript-mode)
  :config
  ;; Enable add-node-modules-path for specific modes
  (dolist (mode '(typescript-mode-hook
                  rjsx-mode-hook
                  json-mode-hook
                  web-mode-hook))
    (add-hook mode #'add-node-modules-path)))

(use-package format-all
  :hook (prog-mode . format-all-mode))

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
  ;; :after (js2-mode rjsx-mode)
  :hook ((js2-mode rjsx-mode typescript-mode) . jest-minor-mode))

(use-package npm-mode
  ;; :after (js2-mode rjsx-mode typescript-mode)
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
  :hook ((web-mode rjsx-mode) . emmet-mode))

(use-package lsp-tailwindcss
  :after (rjsx-mode web-mode)
  :straight (lsp-tailwindcss
             :type git
             :host github
             :repo "merrickluo/lsp-tailwindcss")
  :init
  (setq lsp-tailwindcss-add-on-mode t))

(use-package json-mode
  :mode "\\.json\\'"
  :hook (lsp-deferred . json-mode))

(use-package scss-mode
  :mode "\\.scss\\'"
  :custom
  (scss-compile-at-save nil)
  :config
  (web-mode))

(use-package yaml-mode
  :mode ("\\.yml\\'" "\\.yaml\\'")
  :config
  (evil-define-key 'insert yaml-mode-map "RET" 'newline-and-indent))

(use-package lsp-pyright
  :preface
  (defun my/pyright-start()
    (interactive)
    (require 'lsp-pyright)
    (lsp-deferred))
  :hook (python-mode . my/pyright-start))

(use-package grip-mode
  ;; :bind (:map markdown-mode-command-map
  ;;             ("g" . grip-mode))
  :commands (grip-mode)
  )

(use-package org
  :preface
  (defun sp/org-mode-setup ()
    (org-indent-mode)
    (variable-pitch-mode 1)
    (visual-line-mode 1))
  :commands (org-capture org-agenda)
  :hook (org-mode . sp/org-mode-setup)
  :custom
  (org-ellipsis "  ")
  (org-agenda-start-with-log-mode t)
  (org-log-done 'time)
  (org-log-into-drawer t)
  (org-hide-emphasis-markers t)
  (org-src-fontify-natively t)
  (org-fontify-quote-and-verse-blocks t)
  (org-src-tab-acts-natively t)
  (org-startup-folded 'content)
  (org-src-preserve-indentation nil)
  (org-hide-block-startup nil)
  (org-edit-src-content-indentation 2)
  (org-confirm-babel-evaluate nil)
  (org-cycle-separator-lines 2)
  (org-catch-invisible-edits 'smart)
  (org-export-in-background t)
  (org-use-property-inheritance t)
  (org-directory "~/Documents/Notes/org")
  (org-agenda-files
   '("~/Documents/Notes/org/Tasks.org"
     "~/Documents/Notes/org/Habits.org"
     "~/Documents/Notes/org/Birthdays.org"))
  (org-refile-targets
   '(("Archive.org" :maxlevel . 1)
     ("Tasks.org" :maxlevel . 1)))
  (org-todo-keywords
   '((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d!)")
     (sequence "BACKLOG(b)" "PLAN(p)" "READY(r)" "ACTIVE(a)" "REVIEW(v)" "WAIT(w@/!)" "HOLD(h)" "|" "COMPLETED(c)" "CANC(k@)")))
  (org-habit-graph-column 60)

  :config
  (require 'org-habit)
  (add-to-list 'org-modules 'org-habit)
  ;; Save Org buffers after refiling!
  (advice-add 'org-refile :after 'org-save-all-org-buffers))

(custom-set-faces
 '(org-level-1 ((t (:inherit outline-1 :height 1.20))))
 '(org-level-2 ((t (:inherit outline-2 :height 1.10))))
 '(org-level-3 ((t (:inherit outline-3 :height 1.03))))
 '(org-level-4 ((t (:inherit outline-4 :height 1.0))))
 '(org-level-5 ((t (:inherit outline-5 :height 1.0)))))

;; (general-define-key
;;  :states '(normal visual)
;;  :keymaps 'org-mode-map
;;  "<<" '(org-do-promote :which-key "Promote heading")
;;  ">>" '(org-do-demote :which-key "Demote heading")
;;  "M-h" '(org-do-promote :which-key "Promote heading")
;;  "M-l" '(org-do-demote :which-key "Demote heading")
;;  "M-j" '(org-move-subtree-down :which-key "Move subtree down")
;;  "M-k" '(org-move-subtree-up :which-key "Move subtree up")
;;  [tab] '(evil-toggle-fold :which-key "Toggle Folds")
;;  "g h" '(org-up-element :which-key "Move to up tree")
;;  "g l" '(org-down-element :which-key "Move to down tree")
;;  "g j" '(org-forward-element :which-key "Move to next element")
;;  "g k" '(org-backward-element :which-key "Move to prev element"))
;;
;; (general-define-key
;;  :states '(normal visual)
;;  :keymaps 'org-table-fedit-map
;;  "(" '(org-table-previous-field :which-key "Previous table cell")
;;  ")" '(org-table-next-field :which-key "Next table cell")
;;  "{" '(org-table-beginning-of-field :which-key "Beg. of Table")
;;  "}" '(org-table-end-of-field :which-key "End of table")
;;  "M-h" '(org-table-fedit-ref-left :which-key "Move tbl col left")
;;  "M-l" '(org-table-fedit-ref-right :which-key "Move tbl col right"))

;; (general-define-key
;;  :states '(normal insert)
;;  :keymaps 'org-mode-map
;;  [M-return] #'(lambda()
;;                (interactive)
;;                (org-insert-heading-after-current)
;;                (evil-insert-state)
;;                :which-key "Insert heading below"))

(with-eval-after-load 'org
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((emacs-lisp . t)
     (python . t)))

  (push '("conf-unix" . conf-unix) org-src-lang-modes))

(with-eval-after-load 'org
  (require 'org-tempo)

  (add-to-list 'org-structure-template-alist '("sh" . "src sh"))
  (add-to-list 'org-structure-template-alist '("el" . "src emacs-lisp"))
  (add-to-list 'org-structure-template-alist '("sc" . "src scheme"))
  (add-to-list 'org-structure-template-alist '("ts" . "src typescript"))
  (add-to-list 'org-structure-template-alist '("py" . "src python"))
  (add-to-list 'org-structure-template-alist '("go" . "src go"))
  (add-to-list 'org-structure-template-alist '("yaml" . "src yaml"))
  (add-to-list 'org-structure-template-alist '("json" . "src json")))

(use-package org-bullets
  :hook (org-mode . org-bullets-mode)
  :custom
  (org-bullets-bullet-list '("◉" "○" "●" "○" "●" "○" "●")))

(use-package evil-org
  :hook (org-mode . evil-org-mode))

(use-package org-auto-tangle
  :defer t
  :hook (org-mode . org-auto-tangle-mode))

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

(general-define-key
 :keymaps 'override
 :states  '(emacs normal visual)
 :prefix "SPC r"
 :global-prefix "M-SPC"
 "" '(:ignore t :which-key "Org Roam")
 "f" '(org-roam-node-find :which-key "Node find")
 "i" '(org-roam-node-insert :which-key "Node Insert")
 "l" '(org-roam-buffer-toggle :which-key "Node Buf. Toggle"))

;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;;; Debug
(when doom-debug-p
  (require 'benchmark-init)
  (add-hook 'doom-first-input-hook #'benchmark-init/deactivate))

(defun efs/display-startup-time ()
  (message "Emacs loaded in %s with %d garbage collections."
           (format "%.2f seconds"
                   (float-time
                    (time-subtract after-init-time before-init-time)))
           gcs-done))

(add-hook 'emacs-startup-hook #'efs/display-startup-time)

;;; -----------------------------------------------------------------
;;;|                    Set variables                               |
;;; -----------------------------------------------------------------
(setq user-full-name "Shubham Pawar"
      user-mail-address "shubhampawar3007@gmail.com")

(setq doom-font (font-spec :family "JetBrainsMono Nerd Font" :size 13)
      doom-variable-pitch-font (font-spec :family "JetBrainsMono Nerd Font" :size 14)
      doom-big-font (font-spec :family "FuraCode Nerd Font" :size 22))
(after! doom-themes
  (setq doom-themes-enable-bold t
        doom-themes-enable-italic t))
(custom-set-faces!
  '(font-lock-comment-face :slant italic)
  '(font-lock-keyword-face :slant italic))

(setq doom-theme 'doom-one)
(setq display-line-numbers-type 'relative)

(after! org
  (setq org-directory "~/Documents/Notes/org"
        org-roam-directory (file-truename "~/Documents/Notes/Roam")
        org-ellipsis "  "
        org-superstar-headline-bullets-list '("◉" "●" "○" "◆" "●" "○" "◆")
        org-log-done 'time))

(after! dired
  (setq delete-by-moving-to-trash t
        trash-directory (concat (getenv "XDG_CACHE_HOME") "/dired-trash")))

(after! eshell
  (setq eshell-aliases-file  (concat doom-private-dir "eshell/aliases")
        eshell-history-size 5000
        eshell-buffer-maximum-lines 5000
        eshell-hist-ignoredups t
        eshell-scroll-to-bottom-on-input t
        eshell-destroy-buffer-when-process-dies t
        eshell-visual-commands'("bash" "fish" "htop" "ssh" "top" "zsh")))

(after! vterm
  (setq vterm-max-scrollback 5000))

(after! avy
  (setq avy-single-candidate-jump  t))

(after! centaur-tabs-mode
  (setq centaur-tabs-set-icons t))

;;; -----------------------------------------------------------------
;;;|                    Functions                                   |
;;; -----------------------------------------------------------------
(defun sp/improve-word-length ()
  "This way, when do a 'e' (evil-forward-word-end) it is better.
Even playing with symbol, when inside a string, it becomes a word"
  (modify-syntax-entry ?_ "w")
  (modify-syntax-entry ?- "w"))
(defun sp/save-and-kill-this-buffer ()
  (interactive)
  (save-buffer)
  (kill-this-buffer))

;;; -----------------------------------------------------------------
;;;|                    Hooks                                       |
;;; -----------------------------------------------------------------
(add-hook! 'after-change-major-mode-hook #'sp/improve-word-length)
(add-hook! (web-mode emacs-lisp-mode) #'rainbow-mode)
;;; -----------------------------------------------------------------
;;;|                    Evil-Ex Commands                            |
;;; -----------------------------------------------------------------
(evil-ex-define-cmd "q" #'kill-this-buffer)
(evil-ex-define-cmd "wq" #'sp/save-and-kill-this-buffer)

;;; -----------------------------------------------------------------
;;;|                    Key bindings                                |
;;; -----------------------------------------------------------------
(map! :map evil-insert-state-map
      "C-S-v" #'yank)

(map! :leader
      :desc "Search Projec" ";" '+default/search-project
      :desc "Toggle Comment Line" "/" #'comment-line
      :desc "Flycheck" "t F" #'flycheck-mode
      :desc "Fullscreen Mode" "t f" #'toggle-frame-fullscreen)

(map! :leader
      :map emacs-lisp-mode-map
      :desc "Eval" "e" (:ignore t :which-key "Eval")
      :desc "Buffer" "eb" #'eval-buffer
      :desc "Current Line" "ee" #'eros-eval-last-sexp
      :desc "Function" "ed" #'eros-eval-defun)

(map! :map evil-normal-state-map
      :desc "Goto Line" "C-s" #'consult-line
      ",w" #'save-buffer
      "0" #'evil-first-non-blank
      "s" #'avy-goto-char-2
      "S" #'avy-goto-word-0
      (:when prog-mode-map
       :desc "Next error" "]g" #'flycheck-next-error
       :desc "Prev error" "[g" #'flycheck-previous-error))

(map! :map centaur-tabs-mode-map
      :desc "Prev Tab" "<C-prior>" #'centaur-tabs-backward
      :desc "Next Tab" "<C-next>" 'centaur-tabs-forward)

(map! :map dired-mode-map
      :desc "Prev Dir." "h" 'dired-single-up-directory
      :desc "Show hidden files" "H" 'dired-omit-mode
      :desc "Open file/folder" "l" 'dired-single-buffer
      :desc "Copy" "y" 'dired-ranger-copy
      :desc "Cut" "X" 'dired-ranger-move
      :desc "Paste" "p" 'dired-ranger-paste)

;;; -----------------------------------------------------------------
;;;|                    Use Packages                                |
;;; -----------------------------------------------------------------
(use-package evil-replace-with-register
  :after evil-collection
  :custom
  (evil-replace-with-register-indent t)
  (evil-replace-with-register-key (kbd "x"))
  :config
  (evil-replace-with-register-install)
  (evil-global-set-key 'normal (kbd "X") (kbd "x $")))

(use-package! uniquify
  :defer 2
  :config
    (setq uniquify-buffer-name-style 'forward))

(use-package! lsp-tailwindcss
    :init
    (setq! lsp-tailwindcss-add-on-mode t))
(use-package! dired-single
  :commands (dired dired-jump))

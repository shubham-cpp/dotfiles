;;; early-init.el -*- lexical-binding: t; -*-

;; Native compilation settings
(when (featurep 'native-compile)
  ;; Silence compiler warnings as they can be pretty disruptive
  (eval-and-compile
    (setq native-comp-async-report-warnings-errors nil
	  native-comp-speed 2
	  comp-speed 2
	  comp-async-report-warnings-errors nil
	  native-comp-async-query-on-exit t
	  comp-async-query-on-exit t)
    )

  ;; Make native compilation happens asynchronously
  (setq native-comp-deferred-compilation t)

  ;; Set the right directory to store the native compilation cache
  (add-to-list 'native-comp-eln-load-path (expand-file-name "~/.cache/emacs-my/eln-cache/")))

(eval-and-compile
  (setq gc-cons-threshold most-positive-fixnum
	gc-cons-percentage 0.6
	load-prefer-newer noninteractive
	package-enable-at-startup nil
	file-name-handler-alist nil
	site-run-file nil
	read-process-output-max (* 10 1024 1024)
	bidi-inhibit-bpa t
	highlight-nonselected-windows nil
	frame-inhibit-implied-resize t
	frame-resize-pixelwise t
	fast-but-imprecise-scrolling t
	redisplay-skip-fontification-on-input t))

(setq-default bidi-display-reordering 'left-to-right
	      bidi-paragraph-direction 'left-to-right
	      cursor-in-non-selected-windows nil)

(defvar ian/gc-cons-threshold (* 100 1024 1024))

(setq inhibit-startup-message t)
(push '(tab-bar-mode . 0) default-frame-alist)
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(menu-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)
(push '(background-color . "#232635") default-frame-alist)
(push '(foreground-color . "#FFFFFF") default-frame-alist)
(push '(mouse-color . "white") default-frame-alist)
(push '(font . "JetBrainsMono Nerd Font-11") default-frame-alist)

;; Make the initial buffer load faster by setting its mode to fundamental-mode
(setq initial-major-mode 'fundamental-mode)

(add-hook 'emacs-startup-hook ; hook run after loading init files
          #'(lambda ()
              (setq gc-cons-threshold ian/gc-cons-threshold
                    gc-cons-percentage 0.1)))

(add-hook 'minibuffer-setup-hook #'(lambda ()
                                     (setq gc-cons-threshold most-positive-fixnum)))
(add-hook 'minibuffer-exit-hook #'(lambda ()
                                    (garbage-collect)
                                    (setq gc-cons-threshold ian/gc-cons-threshold)))

(provide 'early-init)
;;; early-init.el ends here

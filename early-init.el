;;; early-init.el — minimal, fast, and tidy startup -*- lexical-binding: t -*-

;; Speed up startup by delaying GC; restore after init
(let ((normal-gc-cons-threshold gc-cons-threshold)
      (normal-gc-cons-percentage gc-cons-percentage))
  (setq gc-cons-threshold most-positive-fixnum
        gc-cons-percentage 0.6)
  (add-hook 'emacs-startup-hook
            (lambda ()
              (setq gc-cons-threshold normal-gc-cons-threshold
                    gc-cons-percentage normal-gc-cons-percentage))))

;; Use external package directory (keeps repo clean)
(setq package-user-dir (expand-file-name "~/src/mine/elpa.d/"))
;; Keep quickstart cache alongside external package dir
(setq package-quickstart-file (expand-file-name "package-quickstart.el" package-user-dir))

;; Avoid package.el auto-loading chaos; keep quickstart cache enabled
(setq package-enable-at-startup t
      package-quickstart t)

;; Keep Custom output out of init.el
(setq custom-file (locate-user-emacs-file "custom.el"))

;; Disable UI elements early for fewer redraws
(push '(menu-bar-lines . 0) default-frame-alist)
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars . nil) default-frame-alist)

;; Don’t flash a GUI frame when using emacsclient
(setq frame-inhibit-implied-resize t
      inhibit-startup-screen t
      inhibit-default-init t
      inhibit-splash-screen t
      inhibit-startup-message t)

;; Load custom settings after startup if present
(add-hook 'after-init-hook
          (lambda ()
            (when (and custom-file (file-readable-p custom-file))
              (load custom-file 'noerror 'nomessage))))

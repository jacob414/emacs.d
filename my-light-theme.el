;;; -*- Mode: Emacs-Lisp -*-
;;; A light color theme I copy-tweaked long ago.
;;;

(provide 'my-light-theme)

(require 'color-theme)

(defun my-color-theme-light ()
  (interactive)
  ;; main theme
  (color-theme-install
   '(my-color-theme-light
     ((background-color . "lightgrey")
      (foreground-color . "black")
      (cursor-color . "black")
      (mouse-color . "white")
      (background-mode . light))
     (default ((t (nil))))
     (bold ((t (:bold t))))
     (bold-italic ((t (:italic t :bold t))))
     (italic ((t (:italic t))))
     ;; red earth: #A73232
     ;; purple: #8722c9
     ;; light green: #228C00
     ;; teal: #008080
     (font-lock-builtin-face ((t (:bold t :foreground "#A73232"))))
     (font-lock-comment-face ((t (:foreground "#8722c9"))))
     (font-lock-constant-face ((t (:bold t :foreground "#398EE6"))))
     (font-lock-doc-string-face ((t (:bold t :foreground "#51B200"))))
     (font-lock-function-name-face ((t (:foreground "black"))))
     (font-lock-keyword-face ((t (:bold t :foreground "#A73232"))))
     (font-lock-preprocessor-face ((t (:foreground "#8722c9" :bold t))))
     (font-lock-reference-face ((t (:foreground "red3"))))
     (font-lock-string-face ((t (:foreground "#228C00"))))
     (font-lock-type-face ((t (:bold t :foreground "#008080"))))
     (font-lock-variable-name-face ((t (:foreground "black"))))
     (font-lock-warning-face ((t (:bold t :foreground "red"))))
     (py-builtins-face ((t (:bold t :foreground "#398EE6"))))
     (py-pseudo-keyword-face ((t (:bold t :foreground "#398EE6"))))
     (rst-level-1-face ((t (:bold t))))
     (rst-level-2-face ((t (:bold t))))
     (rst-level-3-face ((t (:bold t))))
     (rst-level-4-face ((t (:bold t))))
     (highlight-changes ((t (:foreground nil :background "light steel blue"))))
     (highlight-changes-delete ((t (:foreground nil :background "IndianRed3"))))
     (erc-action-face ((t (nil))))
     (erc-notice-face ((t (:foreground "#878899"))))
     (erc-bold-face ((t (:bold t :weight bold))))
     (erc-command-indicator-face ((t (:bold t :weight bold))))
     (erc-dangerous-host-face ((t (:foreground "red"))))
     (erc-default-face ((t (nil))))
     (erc-timestamp-face ((t (:bold nil :foreground "gray45" :weight normal))))
     (erc-underline-face ((t (:underline t))))
     (erc-prompt-face ((t (:bold t :foreground "GoldenRod3" :weight bold))))
     (trailing-whitespace ((t (:background "red")))))))

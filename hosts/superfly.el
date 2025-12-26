;; Relevant PATH --------------------------------------------------------------

(setenv "PATH" (concat (getenv "PATH") ":/usr/local/bin/"))
(setenv "NODE_PATH" "/usr/local/share/npm/lib/node_modules")

;; Common Lisp (via Clozure CL) -----------------------------------------------

(load (expand-file-name "~/quicklisp/slime-helper.el"))
(setq inferior-lisp-program "/usr/local/bin/ccl")

;; Location of MIT-Scheme on this machine -------------------------------------

(setq scheme-program-name "/usr/local/bin/scheme")

;; Final typeface adjustment  -------------------------------------------------

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:height 108 :family "Mensch"))))
 '(linum ((t (:inherit (shadow default) :height 0.8)))))

;; Use server  ----------------------------------------------------------------

(server-start)

;; Go fullscreen immediately  -------------------------------------------------

(ns-toggle-fullscreen)
(split-window-horizontally)
(when (> (frame-width) 272)
 (split-window-horizontally))
(when (> (frame-width) 300)
  (custom-set-faces '(default ((t (:height 120 :family "Mensch")))))
  (split-window-horizontally))
(balance-windows)

;; Pop up a new frame in fullscreen with 2 columns ----------------------------

(defun small-ui ()
  (interactive)
  (new-frame)
  (ns-toggle-fullscreen)
  (split-window-horizontally) )

;; Superfly specific keybingings  ---------------------------------------------

(global-set-key (kbd "M-RET") 'ns-toggle-fullscreen)

(provide 'superfly)

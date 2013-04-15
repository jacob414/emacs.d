;; Relevant PATH --------------------------------------------------------------

(setenv "PATH" (concat (getenv "PATH") ":/usr/local/bin/"))
(setenv "NODE_PATH" "/usr/local/share/npm/lib/node_modules")

;; Dash integration  ----------------------------------------------------------

(autoload 'dash-at-point "dash-at-point"
          "Search the word at point with Dash." t nil)
(global-set-key (kbd "C-c h") 'dash-at-point)

;; Final typeface adjustment  -------------------------------------------------

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:height 110 :family "Mensch"))))
 '(linum ((t (:inherit (shadow default) :height 0.8)))))

;; Use server  ----------------------------------------------------------------

(server-start)

;; Go fullscreen immediately  -------------------------------------------------

(ns-toggle-fullscreen)
(split-window-horizontally)
(when (> (frame-width) 272)
 (split-window-horizontally))
(balance-windows)

;; Pop up a new frame in fullscreen with 2 columns ----------------------------

(defun small-ui ()
  (interactive)
  (new-frame)
  (ns-toggle-fullscreen)
  (split-window-horizontally) )

(provide 'superfly)

;; Relevant PATH --------------------------------------------------------------

(setq brew-base "/usr/local/share/emacs/site-lisp")

(setenv "PATH" (concat (getenv "PATH") ":/usr/local/bin/"))
(setenv "NODE_PATH" "/usr/local/share/npm/lib/node_modules")

;; YASnippet ------------------------------------------------------------------

(add-to-list 'load-path (concat brew-base "/yasnippet"))
(require 'yasnippet)
(yas/load-directory (concat emacs-dir "/snippet"))
(yas/global-mode)
(global-set-key (kbd "C-c y") 'yas/reload-all)

;; Common Lisp (via Clozure CL) -----------------------------------------------

;;(load (expand-file-name "~/quicklisp/slime-helper.el"))
;;(setq inferior-lisp-program "/usr/local/bin/ccl")

;; Magit ----------------------------------------------------------------------

(require 'magit)

;; Haskell --------------------------------------------------------------------

(require 'haskell-mode)
(setq auto-mode-alist
      (append '(("\\.hs$" . sass-mode)) auto-mode-alist))

;; Location of MIT-Scheme on this machine -------------------------------------

(setq scheme-program-name "/usr/local/bin/scheme")

;; Dash integration  ----------------------------------------------------------

(autoload 'dash-at-point "dash-at-point"
          "Search the word at point with Dash." t nil)
(global-set-key (kbd "C-c h") 'dash-at-point)

;; Final typeface adjustment  -------------------------------------------------

(custom-set-faces
 '(default ((t (:height 115 :family "Operator Mono"))))
 '(linum ((t (:inherit (shadow default) :height 0.9)))))

;; Use server  ----------------------------------------------------------------

(server-start)
(remove-hook 'kill-buffer-query-functions 'server-kill-buffer-query-function)

;; Go fullscreen immediately  -------------------------------------------------

(toggle-frame-fullscreen)
(split-window-horizontally)
(when (> (frame-width) 272)
 (split-window-horizontally))
(when (> (frame-width) 300)
  (custom-set-faces '(default ((t (:height 115 :family "Operator Mono")))))
  (split-window-horizontally))
(balance-windows)

;; Pop up a new frame in fullscreen with 2 columns ----------------------------

(defun small-ui ()
  (interactive)
  (new-frame)
  (toggle-frame-fullscreen)
  (split-window-horizontally) )

;; Sugarline specific keybingings  ---------------------------------------------

(global-set-key (kbd "M-RET") 'toggle-frame-fullscreen)

;; Sugarline specific paths/language settings ----------------------------------

(add-to-list 'load-path "~/src/mine/skunkworks/emacs")
(require 'diasend-sugarline)

;; Final custom variables (last to win) ---------------------------------------

(custom-set-variables
 '(elm-format-command "/usr/local/bin/elm-format")
 '(elm-interactive-command "/usr/local/bin/elm-repl")
 '(elm-format-on-save 't)
 '(nodejs-repl-command "/usr/local/bin/node")
)

(my-zenburn)

(message "sugarline setup done")

(provide 'sugarline)

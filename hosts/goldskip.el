;; Common Lisp (via Clozure CL) -----------------------------------------------

(load (expand-file-name "~/quicklisp/slime-helper.el"))
(setq inferior-lisp-program "/usr/local/bin/ccl")

;; Magit ----------------------------------------------------------------------

(require 'magit)

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

;; Pop up a new frame in fullscreen with 2 columns --------------------------

(unless (package-installed-p 'langtool)
  (package-install 'langtool))

(setq langtool-bin "/usr/local/bin/languagetool")
(setq langtool-default-language "sv")
(require 'langtool)


;; Pop up a new frame in fullscreen with 2 columns ----------------------------

(defun small-ui ()
  (interactive)
  (new-frame)
  (toggle-frame-fullscreen)
  (split-window-horizontally) )

;; Goldskip specific keybingings  ---------------------------------------------

;; Empty fn.

;; Goldskip specific paths/language settings ----------------------------------

(custom-set-variables
 '(elm-format-command "/usr/local/bin/elm-format")
 '(elm-interactive-command "/usr/local/bin/elm-repl")
 '(elm-format-on-save 't)
 '(nodejs-repl-command "/usr/local/bin/node")
)

(message "goldskip setup done")

(provide 'goldskip)

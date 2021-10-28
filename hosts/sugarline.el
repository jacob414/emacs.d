;; Python --------------------------------------------------------------------

(setq python-shell-interpreter-args "-i /Users/jacob/src/mine/emacs.d/pythonstart.py")

;; YASnippet ------------------------------------------------------------------

(require 'yasnippet)
(yas/load-directory (concat emacs-dir "/snippet"))
(yas/global-mode)
(global-set-key (kbd "C-c y") 'yas/reload-all)
;; (add-to-list 'warning-suppress-types '(yasnippet backquote-change))

(setq yas-snippet-dirs
      '("~/src/mine/emacs.d/snippet"  ;; personal snippets
        ))

(define-key yas-minor-mode-map (kbd "SPC") yas-maybe-expand)

;; Common Lisp (via Clozure CL) -----------------------------------------------

;;(load (expand-file-name "~/quicklisp/slime-helper.el"))
;;(setq inferior-lisp-program "/usr/local/bin/ccl")

;; Haskell --------------------------------------------------------------------

(require 'haskell-mode)
(setq auto-mode-alist
      (append '(("\\.hs$" . sass-mode)) auto-mode-alist))

;; Location of MIT-Scheme on this machine -------------------------------------

(setq scheme-program-name "/usr/local/bin/scheme")

;; Final typeface adjustment  -------------------------------------------------

(defun my-font-setting ()
  "Docstring for my-font-setting."
  (interactive)
  (custom-set-faces
   '(default ((t (:height 115 :family "Monaco"))))
   '(linum ((t (:inherit (shadow default) :height 0.9)))))
  )

(my-font-setting)

;; Use server  ----------------------------------------------------------------

(server-start)
(remove-hook 'kill-buffer-query-functions 'server-kill-buffer-query-function)

;; Go fullscreen immediately  -------------------------------------------------

(toggle-frame-fullscreen)
(split-window-horizontally)
(when (> (frame-width) 272)
 (split-window-horizontally))
(when (> (frame-width) 300)
  (my-font-setting)
  (split-window-horizontally))
(balance-windows)

;; Pop up a new frame in fullscreen with 2 columns ----------------------------

(defun small-ui ()
  (interactive)
  (new-frame)
  (toggle-frame-fullscreen)
  (split-window-horizontally) )

;; Sugarline specific keybingings  ---------------------------------------------

(global-set-key (kbd "C-x n") 'smerge-next)
(global-set-key (kbd "C-x p") 'smerge-next)
(global-set-key (kbd "C-x <down>") 'smerge-keep-lower)
(global-set-key (kbd "C-x <up>") 'smerge-keep-upper)
(global-set-key (kbd "C-c sv") 'my-svenskakoll)
(global-set-key (kbd "C-c en") 'my-en-check)

;; Text mode ------------------------------------------------------------------

(unless (package-installed-p 'langtool)
  (package-install 'langtool))

(setq langtool-bin "/usr/local/bin/languagetool")
(setq langtool-default-language "sv")
(require 'langtool)


;; Sugarline specific paths/language settings ----------------------------------

(add-to-list 'load-path "~/src/mine/skunkworks/emacs")
;; (require 'diasend-sugarline)

;; Final custom variables (last to win) ---------------------------------------

(custom-set-variables
 '(elm-format-command "/usr/local/bin/elm-format")
 '(elm-interactive-command "/usr/local/bin/elm-repl")
 '(elm-format-on-save 't)
 '(nodejs-repl-command "/usr/local/bin/node")
)

(message "sugarline setup done")

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:height 115 :family "Monaco"))))
 '(cursor ((t (:background "red"))))
 '(highlight-current-line-face ((t (:background "gray35"))))
 '(linum ((t (:inherit (shadow default) :height 0.9))))
 '(mode-line ((t (:background "Black" :foreground "#a4a097" :inverse-video t :box nil :underline nil :slant normal :weight normal))))
 '(mode-line-highlight ((t (:box (:line-width 2 :color "#d1cdc1" :style released-button)))))
 '(mode-line-inactive ((t (:background "Brown" :foreground "#d1cdc1" :inverse-video t :box nil :underline nil :slant normal :weight normal))))
 '(org-checkbox ((t (:foreground nil :inherit org-todo))))
 '(org-link ((t (:foreground "#21867a" :underline t)))))


;; Tuareg (OCaml) configuration -----------------------------------------------

(load "~/src/ext/ocaml/tuareg/tuareg-site-file")


(provide 'sugarline)

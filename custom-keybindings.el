;; Always indent
(global-set-key (kbd "C-m") 'newline-and-indent)

;; Custom key bindings
(global-set-key (kbd "C-<tab>") 'dabbrev-expand)
(global-set-key (kbd "C-z") 'goto-line)
(global-set-key (kbd "C-c j") 'goto-line)
(global-set-key (kbd "C-c C-j") 'goto-line)
(global-set-key (kbd "C-(") 'insert-parentheses)
(global-set-key (kbd "C-c i") 'indent-region)
(global-set-key (kbd "C-c f") 'forward-sexp)
(global-set-key (kbd "<f5>") 'eval-region)
(global-set-key (kbd "C-c p") 'query-replace)
(global-set-key (kbd "C-c a") 'mark-whole-buffer)
(global-set-key (kbd "C-c f") 'rgrep)
(global-set-key (kbd "C-c o") 'occur)
(global-set-key (kbd "C-c k") 'kill-whole-line)
(global-set-key (kbd "C-c m t") 'ws-trim-mode)
(global-set-key (kbd "C-c m l") 'longlines-mode)
(global-set-key (kbd "C-x <SPC>") 'whitespace-mode)
(global-set-key (kbd "C-c T") 'ws-trim-buffer)
(global-set-key (kbd "C-=") 'er/expand-region)
(global-set-key (kbd "C-x RET") 'hs-toggle-hiding)
(global-set-key (kbd "C-x x") 'eval-expression)

;; Mixin in some paredit
(global-set-key (kbd "M-'") 'paredit-open-curly)
(global-set-key (kbd "C-8") 'paredit-open-round)
(global-set-key (kbd "C-M-(") 'paredit-open-curly)
(global-set-key (kbd "C-\"") 'paredit-doublequote)

;; Magit

(global-set-key (kbd "C-x g") 'magit-status)

;; Paredit
(global-set-key (kbd "M-D") 'paredit-kill)
(global-set-key (kbd "C-x 9") 'paredit-close-parenthesis)

;; Fullscreen
(global-set-key (kbd "s-<return>") 'toggle-frame-fullscreen)

(global-set-key (kbd "C-x C-m") 'hs-toggle-hiding)

;; Evaluate expression
(global-set-key (kbd "M-E") 'eval-expression)

(provide 'custom-keybindings)

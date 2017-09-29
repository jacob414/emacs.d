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
(global-set-key (kbd "C-å") 'er/expand-region)
(global-set-key (kbd "C-x RET") 'hs-toggle-hiding)

;; Mixin in some paredit
(global-set-key (kbd "M-'") 'paredit-open-curly)
(global-set-key (kbd "C-8") 'paredit-open-round)
(global-set-key (kbd "C-M-(") 'paredit-open-curly)
(global-set-key (kbd "C-\"") 'paredit-doublequote)

;; Magit
(global-set-key (kbd "C-x g") 'magit-status)
(global-set-key (kbd "C-x ?") 'magit-diff-buffer-file)

;; Paredit
(global-set-key (kbd "M-D") 'paredit-kill)
(global-set-key (kbd "C-x 9") 'paredit-close-parenthesis)

(provide 'custom-keybindings)

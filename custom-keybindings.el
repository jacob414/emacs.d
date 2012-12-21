;; Always indent
(global-set-key (kbd "C-m") 'newline-and-indent)

;; Custom key bindings
(global-set-key (kbd "C-<tab>") 'dabbrev-expand)
(global-set-key (kbd "C-z") 'goto-line)
(global-set-key (kbd "C-c j") 'goto-line)
(global-set-key (kbd "C-c C-j") 'goto-line)
(global-set-key (kbd "C-(") 'insert-parentheses)
(global-set-key (kbd "C-c i") 'indent-region)
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

(provide 'j414-custom-keybindings)

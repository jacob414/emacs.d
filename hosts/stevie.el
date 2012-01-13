(message "Stevie specific..")

;; Priset för riktig Meta -tangent
(global-set-key (kbd "M-4") "$")
(global-set-key (kbd "M-7") "{")
(global-set-key (kbd "M-8") "[")
(global-set-key (kbd "M-9") "]")
(global-set-key (kbd "M-0") "}")
(global-set-key (kbd "M-2") "@")
(global-set-key (kbd "M-+") "\\")
(global-set-key (kbd "§") "<")
(global-set-key (kbd "°") ">")
(global-set-key (kbd "<end>") 'end-of-line)
(global-set-key (kbd "<home>") 'beginning-of-line)

(require 'zenburn)
(zenburn)

(highlight-current-line-set-bg-color "gray33")

(custom-set-faces
'(default ((t (:height 110 :family "menlo")))) )

(set-face-background 'show-paren-match-face "grey40")
(set-face-attribute 'show-paren-match-face nil
       :weight 'bold :underline nil :overline nil :slant 'normal)

(global-set-key (kbd "M-RET") 'ns-toggle-fullscreen)
(provide 'stevie)

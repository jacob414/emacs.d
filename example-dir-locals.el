;; Example .dir-locals.el file
;; This file demonstrates how to use directory-local variables safely
;; Copy this to any project directory and rename it to .dir-locals.el

;; Settings for all files in this directory
((nil . ((indent-tabs-mode . nil)
         (tab-width . 4)
         (fill-column . 80)))
 
 ;; Settings specific to Python files
 (python-mode . ((python-indent-offset . 4)))
 
 ;; Settings specific to web files
 (web-mode . ((web-mode-markup-indent-offset . 2)
              (web-mode-css-indent-offset . 2)
              (web-mode-code-indent-offset . 2)))
 
 ;; Settings specific to org files
 (org-mode . ((org-confirm-babel-evaluate . t)))
 
 ;; Settings for markdown files
 (markdown-mode . ((fill-column . 72)))
 
 ;; Settings for JavaScript files
 (js-mode . ((js-indent-level . 2))))

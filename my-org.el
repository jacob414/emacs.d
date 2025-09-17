;; My org settings - split into own module for readability.

(require 'ox-html)

;; Set default publishing options
(setq org-html-validation-link nil ;; Remove validation link
      org-html-doctype "html5"
      org-html-html5-fancy t
      org-html-postamble nil ;; No footer
      org-html-head-include-scripts nil ;; Don't include default scripts
      org-html-head-include-default-style nil ;; Don't include default style
      org-html-head "<link rel=\"stylesheet\" type=\"text/css\" href=\"style.css\" />")

;; Define a function to publish your site
(defun my-org-publish-site ()
  "Publish the Org site and copy the style.css file."
  (interactive)
  (let ((org-publish-project-alist
         '(("my-site"
            :base-directory "~/src/mine/site"
            :publishing-directory "~/src/tmp/site"
            :recursive t
            :publishing-function org-html-publish-to-html
            :with-toc nil))))
    ;; Publish the Org files
    (org-publish-all t)

    ;; Copy the style.css file to the publishing directory
    (let* ((source-css-file
            (expand-file-name "style.css" "~/src/mine/site"))
           (destination-css-file
            (expand-file-name "style.css" "~/src/tmp/site")))
      (when (file-exists-p source-css-file)
        (copy-file source-css-file destination-css-file t)))))

;; Bind the function to a key combination if desired
(global-set-key (kbd "C-c p") 'my-org-publish-site)

;; Deploy static site

;; Define a function to publish your site and upload it using a shell script
(defun my-org-publish-site ()
  "Publish the Org site, copy the style.css file, and run the upload script."
  (interactive)
  ;; Publish the Org files
  (let ((org-publish-project-alist
         '(("my-site"
            :base-directory "~/src/mine/site"
            :publishing-directory "~/src/tmp/site"
            :recursive t
            :publishing-function org-html-publish-to-html
            :with-toc nil))))
    (org-publish-all t)

    ;; Copy the style.css file to the publishing directory
    (let* ((source-css-file
            (expand-file-name "style.css" "~/src/mine/site"))
           (destination-css-file
            (expand-file-name "style.css" "~/src/tmp/site")))
      (when (file-exists-p source-css-file)
        (copy-file source-css-file destination-css-file t))))

  ;; Run the upload script
  (let ((upload-script "~/bin/414pub.sh"))
    (if (file-executable-p upload-script)
        (shell-command (concat "bash " upload-script))
      (message "Upload script %s is not executable or does not exist." upload-script))))

(defun my-org-literal ()
  "Makes current selection an org-mode literal."
  (interactive)
  (unless (use-region-p)
    (user-error "No active region: select text to wrap"))
  (org-emphasize ?=)
)

;; #+LaTeX_CLASS: beamer in org files
(unless (boundp 'org-export-latex-classes)
  (setq org-export-latex-classes nil))
(add-to-list 'org-export-latex-classes
  ;; beamer class, for presentations
  '("beamer"
     "\\documentclass[11pt]{beamer}\n
      \\mode<{{{beamermode}}}>\n
      \\usetheme{{{{beamertheme}}}}\n
      \\usecolortheme{{{{beamercolortheme}}}}\n
      \\beamertemplateballitem\n
      \\setbeameroption{show notes}
      \\usepackage[utf8]{inputenc}\n
      \\usepackage[T1]{fontenc}\n
      \\usepackage{hyperref}\n
      \\usepackage{color}
      \\usepackage{listings}
      \\lstset{numbers=none,language=[ISO]C++,tabsize=4,
  frame=single,
  basicstyle=\\small,
  showspaces=false,showstringspaces=false,
  showtabs=false,
  keywordstyle=\\color{blue}\\bfseries,
  commentstyle=\\color{red},
  }\n
      \\usepackage{verbatim}\n
      \\institute{{{{beamerinstitute}}}}\n
       \\subject{{{{beamersubject}}}}\n"

     ("\\section{%s}" . "\\section*{%s}")

     ("\\begin{frame}[fragile]\\frametitle{%s}"
       "\\end{frame}"
       "\\begin{frame}[fragile]\\frametitle{%s}"
       "\\end{frame}")))

  ;; letter class, for formal letters

  (add-to-list 'org-export-latex-classes

  '("letter"
     "\\documentclass[11pt]{letter}\n
      \\usepackage[utf8]{inputenc}\n
      \\usepackage[T1]{fontenc}\n
      \\usepackage{color}"

     ("\\section{%s}" . "\\section*{%s}")
     ("\\subsection{%s}" . "\\subsection*{%s}")
     ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
     ("\\paragraph{%s}" . "\\paragraph*{%s}")
     ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))

(fset 'my-org-point-add
      [return tab backspace backspace ?- ?  ?\M-8 ?  ?\M-9 ? ])

;; http://orgmode.org/manual/Conflicts.html
(add-hook 'org-mode-hook
          (lambda ()
            (local-set-key (kbd "M-<up>") 'windmove-up)
            (local-set-key (kbd "M-<left>") 'windmove-left)
            (local-set-key (kbd "M-<right>") 'windmove-right)
            (local-set-key (kbd "M-<down>") 'windmove-down)
            (local-set-key (kbd "C-<tab>") 'dabbrev-expand)
            (local-set-key (kbd "C-.") 'scroll-down-one-line)
            (local-set-key (kbd "C-,") 'scroll-up-one-line)
            (local-set-key (kbd "C-j") 'my-greedy-joinlines)
            (local-set-key (kbd "C-x C-e") 'langtool-correct-buffer)
            (define-key yas/keymap [tab] 'yas/next-field-or-maybe-expand)
            (setcar (nthcdr 4 org-emphasis-regexp-components) 4)
            ))


(setq org-file-apps
    '(("\\.docx?\\'" . default)
      ("\\.xlsx?\\'" . default)
      ("\\.x?html?\\'" . default)
      ("\\.pdf\\'" . default)
      ("\\.log\\'" . emacs)
      (".*::\\(editme\\)\\'" . (find-file file))
      (auto-mode . emacs)))

(local-set-key (kbd "C-c C-c") 'org-latex-export-to-pdf)
(local-set-key (kbd "C-c C-g") 'my-org-publish-site)

 ;; Stenkoll org-mode integration
(with-eval-after-load 'org
  ;; Load stenkoll integration
  (require 'stenkoll)

  ;; The stenkoll integration now loads automatically - no hook needed
  ;; The new implementation uses defensive loading that won't break org-mode

  (define-key org-mode-map (kbd "C-c C-p") #'my-org-prompt)
  (define-key org-mode-map (kbd "C-c C-+") #'my-org-literal)


  (require 'org-side-tree)
  (require 'org-beautify-theme)

  ;; Optional: Global keybinding for stenkoll commands
  (global-set-key (kbd "C-c s e") 'stenkoll-edit-issue-at-point)
  (global-set-key (kbd "C-c s l") 'stenkoll-list-issues))

(provide 'my-org)

(defun count-all ()
  (interactive)
  (count-lines-region (point-min) (point-max)) )

(global-set-key (kbd "C-x w") 'count-all)

(defun scroll-up-one-line()
  (interactive)
  (scroll-up 1))

(defun scroll-down-one-line()
  (interactive)
  (scroll-down 1))

(global-set-key (kbd "C-.") 'scroll-down-one-line)
(global-set-key (kbd "C-,") 'scroll-up-one-line)

(defun duplicate-current-line ()
  (interactive)
  (beginning-of-line nil)
  (let ((b (point)))
    (end-of-line nil)
    (copy-region-as-kill b (point)))
  (beginning-of-line 2)
  (open-line 1)
  (yank)
  (back-to-indentation))

(global-set-key "\C-cd" 'duplicate-current-line)

(defadvice kill-ring-save (before slick-copy activate compile)
  "When called interactively with no active region, copy a single line instead."
  (interactive
	(if mark-active
	  (list (region-beginning) (region-end))
	  (message "Copied line")
	  (list (line-beginning-position) (line-beginning-position 2))
	)
  )
)

(defun kill-other-buffers ()
      "Kill all other buffers."
      (interactive)
      (mapc 'kill-buffer (delq (current-buffer) (buffer-list))))

(defun my-greedy-joinlines ()
  (interactive)
  (kill-line)
  (just-one-space) )

(global-set-key (kbd "C-j") 'my-greedy-joinlines)

(global-set-key (kbd "C-c l") 'my-longlines)

(defun my-commit-buffer ()
  "Saves buffer and tells server it's done with it"
  (interactive)
  (save-buffer)
  (server-edit) )

(global-set-key (kbd "C-c <right>") 'my-commit-buffer)
(global-set-key (kbd "M-C") 'my-commit-buffer)

(defun my-isodate ()
  (interactive)
  (insert (format-time-string "%Y-%m-%d"))
)

(defun my-isotimestamp ()
  (interactive)
  (insert (format-time-string "%Y-%m-%d %H:%M")) )

(defun my-longyear ()
  (interactive)
  (insert (format-time-string "%Y")) )

(defun po-wrap ()
  "Filter current po-mode buffer through `msgcat' tool to wrap all lines."
  (interactive)
  (if (eq major-mode 'po-mode)
      (let ((tmp-file (make-temp-file "po-wrap."))
     	    (tmp-buf (generate-new-buffer "*temp*")))
     	(unwind-protect
     	    (progn
     	      (write-region (point-min) (point-max) tmp-file nil 1)
     	      (if (zerop
     		   (call-process
     		    "msgcat" nil tmp-buf t (shell-quote-argument tmp-file)))
     		  (let ((saved (point))
     			(inhibit-read-only t))
     		    (delete-region (point-min) (point-max))
     		    (insert-buffer tmp-buf)
     		    (goto-char (min saved (point-max))))
     		(with-current-buffer tmp-buf
     		  (error (buffer-string)))))
     	  (kill-buffer tmp-buf)
     	  (delete-file tmp-file)))))

;;
;; Revert buffer
;;
(defun force-revert-buffer () (interactive)
 (revert-buffer nil t))

(global-set-key "\C-cR" 'force-revert-buffer)

(defun my-ispell-use-sv ()
  (interactive)
  (ispell-change-dictionary "svenska")
)

(defun my-ispell-use-en ()
  (interactive)
  (ispell-change-dictionary "en_GB-ize")
)

(defun my-svenskakoll ()
  (interactive)
  (langtool-check "sv")
)

(defun my-en-check ()
  (interactive)
  (langtool-check "en-GB")
)


(defun comment-or-uncomment-current-line-or-region ()
  "Comments or uncomments current current line or whole lines in region."
  (interactive)
  (save-excursion
    (let (min max)
      (if (and transient-mark-mode mark-active)
          (setq min (region-beginning) max (region-end))
        (setq min (point) max (point)))
      (comment-or-uncomment-region
       (progn (goto-char min) (line-beginning-position))
       (progn (goto-char max) (line-end-position))))))

(global-set-key (kbd "<f9>") 'comment-or-uncomment-current-line-or-region)
(global-set-key (kbd "C-#") 'comment-or-uncomment-current-line-or-region)

;; 90aebf38-b33a-314b-1198-c9bffea2f2a2
(defun uuid-create ()
  "Return a newly generated UUID. This uses a simple hashing of variable data."
  (let ((s (md5 (format "%s%s%s%s%s%s%s%s%s%s"
                        (user-uid)
                        (emacs-pid)
                        (system-name)
                        (user-full-name)
                        user-mail-address
                        (current-time)
                        (emacs-uptime)
                        (garbage-collect)
                        (random)
                        (recent-keys)))))
    (format "%s-%s-3%s-%s-%s"
            (substring s 0 8)
            (substring s 8 12)
            (substring s 13 16)
            (substring s 16 20)
            (substring s 20 32))))

(defun uuid-insert ()
  "Inserts a new UUID at the point."
  (interactive)
  (insert (uuid-create)))

(defun my-muggle-text-mode ()
  "Docstring for my-muggle-text-mode."
  (interactive)
  (visual-line-mode 1)
  (auto-fill-mode 0)
  (ws-trim-mode 0)
  (writegood-mode 1)
  (local-set-key (kbd "C-x L") 'langtool-check)
  )

(defun my-langtool-sv ()
  "Docstring for langtool-svenskakoll."
  (langtool-check "sv") )

(defun pkg-paths (sys-pkg-base)
  "Docstring for pkg-paths."
  (interactive)
  (add-to-list 'load-path (concat sys-pkg-base "/yasnippet"))
  (add-to-list 'load-path (concat sys-pkg-base "/magit"))
  (add-to-list 'load-path (concat sys-pkg-base "/haskell-mode"))
  (add-to-list 'load-path (concat sys-pkg-base "/rainbow-delimiters"))
  (add-to-list 'load-path (concat sys-pkg-base "/w3"))
  (add-to-list 'load-path (concat sys-pkg-base "/multiple-cursors"))
  )

(defun osx-support ()
  (interactive)
  ;; The price of having no real Meta-key
  (global-set-key (kbd "M-0") "≈")
  (global-set-key (kbd "M-4") "$")
  (global-set-key (kbd "M-7") "|")
  (global-set-key (kbd "M-8") "[")
  (global-set-key (kbd "M-9") "]")
  (global-set-key (kbd "M-(") "{")
  (global-set-key (kbd "M-)") "}")
  (global-set-key (kbd "M-)") "}")
  (global-set-key (kbd "M-2") "@")
  (global-set-key (kbd "M-2") "@")
  (global-set-key (kbd "M-+") "\\")
  (global-set-key (kbd "C-c b") "\·")
  (global-set-key (kbd "§") "<")
  (global-set-key (kbd "°") ">")
  (global-set-key (kbd "M-\"") "”")
  (global-set-key (kbd "M-ö") "“")
  (global-set-key (kbd "M-ä") "”")
  (global-set-key (kbd "<end>") 'end-of-line)
  (global-set-key (kbd "<home>") 'beginning-of-line)
  (global-set-key (kbd "<C-s-268632070>") 'toggle-frame-fullscreen)
  (setq exec-path (append exec-path '("/usr/local/bin")))

  ;; OS X typical paths - assumes brew package manager
  (setenv "PATH" (concat (getenv "PATH") ":/usr/local/bin/"))
  (setenv "NODE_PATH" "/usr/local/share/npm/lib/node_modules")

  (setq brew-base "/usr/local/share/emacs/site-lisp")
  (pkg-paths "/usr/local/share/emacs/site-lisp")

  ;; Dodge Python character set trouble
  (setenv "LC_ALL" "en_GB.UTF-8")

  (my-yasnippet)
  )

(defun my-longlines ()
  "Shortcut for my settings for writing text without soft wraps"
  (interactive)
  (ws-trim-mode)
  (visual-line-mode)
  (auto-fill-mode -1) )

(defun string/starts-with (string prefix)
      "Return t if STRING starts with prefix."
      (and (string-match (rx-to-string `(: bos ,prefix) t)
                         string)
           t))

(defun bukharin-mode ()
  "sigh. Because world."
  (interactive)
  (subword-mode)
  (setq indent-tabs-mode t)
  (ws-trim-mode -1) )

(defun tab-emergency ()
  "In the unfortunate event of a source with tab indent"
  (interactive)
  (setq-local tab-width 2)
  (setq-local indent-tabs-mode t) )
(global-set-key (kbd "C-c T") 'tab-emergency)

(defun my-archive (&optional thing)
  "Docstring for archive."
  (interactive)
  (unless thing (setq thing (read-string "Archive:")) )
  (with-current-buffer
    (set-buffer (find-file "~/src/mine/skunkworks/inkorg.org"))
    (goto-char (point-max))
    (insert (concat thing "\n"))
    (save-buffer)
    )
  )

(defun my-org-link (url display)
  "Formats an org-link from an URL"
  (insert (concat "-> [[" url "][" display "]]")) )

(global-set-key (kbd "C-x a") 'my-archive)

(defun shell-results (&optional cmd)
  "Docstring for shell-results."
  (interactive)
  (unless cmd (setq cmd (read-string "Cmd:")) )
  (insert (shell-command-to-string (concat "printf %s $(" cmd ")")))
  )

(defun rename-current-buffer-file ()
  "Renames current buffer and file it is visiting."
  (interactive)
  (let* ((name (buffer-name))
         (filename (buffer-file-name))
         (basename (file-name-nondirectory filename)))
    (if (not (and filename (file-exists-p filename)))
        (error "Buffer '%s' is not visiting a file!" name)
      (let
          ((new-name
            (read-file-name "New name: "
                            (file-name-directory filename)
                            basename nil basename)))
        (if (get-buffer new-name)
            (error "A buffer named '%s' already exists!" new-name)
          (rename-file filename new-name 1)
          (rename-buffer new-name)
          (set-visited-file-name new-name)
          (set-buffer-modified-p nil)
          )
        )
      )
    )
  )

(defun close-all-parentheses ()
  (interactive "*")
  (let ((closing nil))
    (save-excursion
      (while (condition-case nil
         (progn
           (backward-up-list)
           (let ((syntax (syntax-after (point))))
             (case (car syntax)
               ((4) (setq closing (cons (cdr syntax) closing)))
               ((7 8) (setq closing (cons (char-after (point)) closing)))))
           t)
           ((scan-error) nil))))
    (apply #'insert (nreverse closing))))

(provide 'functions)

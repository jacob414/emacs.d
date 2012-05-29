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

(defun my-longlines ()
  (interactive)
  (auto-fill-mode)
  (longlines-mode) )

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
  (my-ispell-use-sv)
  (ispell)
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

(defun osx-support ()
  (interactive)
  ;; The price of having a real Meta-key
  (global-set-key (kbd "M-4") "$")
  (global-set-key (kbd "M-7") "|")
  (global-set-key (kbd "M-8") "[")
  (global-set-key (kbd "M-9") "]")
  (global-set-key (kbd "M-(") "{")
  (global-set-key (kbd "M-)") "}")
  (global-set-key (kbd "M-2") "@")
  (global-set-key (kbd "M-+") "\\")
  (global-set-key (kbd "§") "<")
  (global-set-key (kbd "°") ">")
  (global-set-key (kbd "<end>") 'end-of-line)
  (global-set-key (kbd "<home>") 'beginning-of-line)
  ;; Fullscreen
  (global-set-key (kbd "M-RET") 'ns-toggle-fullscreen)
  )

(provide 'functions)

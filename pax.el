(when (file-directory-p "privates.el") (require 'privates) )

;; Set default publishing options
(setq org-html-validation-link nil ;; Remove validation link
      org-html-doctype "html5"
      org-html-html5-fancy t
      org-html-postamble nil ;; No footer
      org-html-head-include-scripts nil ;; Don't include default scripts
      org-html-head-include-default-style nil ;; Don't include default style
      org-html-head "<link rel=\"stylesheet\" type=\"text/css\" href=\"style.css\" />")

(defun org-shell-region (beg end)
  "Wrap the current region with org-mode shell source block markers.
The region between BEG and END will be surrounded by #+BEGIN_SRC shell
and #+END_SRC markers."
  (interactive "r")
  (save-excursion
    ;; Add end marker after the region
    (goto-char end)
    (end-of-line)
    ;; Only insert newline if we're not already at one
    (unless (or (eobp) (looking-at "\n"))
      (insert "\n"))
    (insert "#+END_SRC")
    ;; Add begin marker before the region
    (goto-char beg)
    (beginning-of-line)
    (insert "#+BEGIN_SRC shell\n")))


(defun my-org-prompt ()
  "Docstring for my-org-prompt."
  (interactive)
  (insert (format-time-string "** %Y-%m-%d %H:%M"))
  )

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

(defun org-shell-region (beg end)
  "Wrap the current region with org-mode shell source block markers.
The region between BEG and END will be surrounded by #+BEGIN_SRC shell
and #+END_SRC markers."
  (interactive "r")
  (save-excursion
    ;; Add end marker after the region
    (goto-char end)
    (end-of-line)
    ;; Only insert newline if we're not already at one
    (unless (or (eobp) (looking-at "\n"))
      (insert "\n"))
    (insert "#+END_SRC")
    ;; Add begin marker before the region
    (goto-char beg)
    (beginning-of-line)
    (insert "#+BEGIN_SRC shell\n")))

 (with-eval-after-load 'org
   (define-key org-mode-map (kbd "C-c #") 'org-shell-region))

(with-eval-after-load 'org
  (define-key org-mode-map (kbd "s-i") 'org-mark-region-slashes))

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

(defvar yank-strip-chars--n 0
  "Internal: number of characters to strip per line for `yank-strip-chars'.")

(defun yank-strip-chars--strip-per-line (s n)
  "Return S with N chars removed from each line.
If N > 0, remove from the *start* of each line.
If N < 0, remove -N chars from the *end* of each line."
  (with-temp-buffer
    (insert s)
    (goto-char (point-min))
    (while (not (eobp))
      (let ((bol (line-beginning-position))
            (eol (line-end-position)))
        (cond
         ((>= n 0)
          (delete-region bol (min (+ bol n) eol)))
         (t
          (let* ((k (- n)) ; k > 0
                 (start (max bol (- eol k))))
            (delete-region start eol)))))
      (forward-line 1))
    (buffer-string)))

(defun yank-strip-chars--transform (string)
  "Transformer used via `yank-transform-functions'."
  (yank-strip-chars--strip-per-line string yank-strip-chars--n))

(defun yank-strip-chars (n)
  "Yank from the kill ring, but first remove N characters from each line.

With a prefix arg, use it as N.
Without a prefix arg, prompt for N.

If N is negative, remove -N characters from the end of each line."
  (interactive
   (list (if current-prefix-arg
             (prefix-numeric-value current-prefix-arg)
           (read-number "Remove how many chars from each line? " 0))))
  (let ((yank-strip-chars--n n)
        (yank-transform-functions
         (cons #'yank-strip-chars--transform yank-transform-functions)))
    (yank)))

(defun pax/yank-log-entry ()
  "Docstring for pax/yank-entry."
  (interactive)
  (newline)
  (yank-strip-chars 2)
  )

(defun pax/yank-to-log ()
  "Docstring for pax/create-log-entry."
  (interactive)
  (my-org-prompt)
  (newline)
  (pax/yank-log-entry)
  )

;; Org-specific keybindings
;; Avoid global C-c p conflicts; bind within org-mode instead.
(with-eval-after-load 'org
  (define-key org-mode-map (kbd "C-c y") #'pax/yank-to-log)
  (define-key org-mode-map (kbd "C-c C-g") #'my-org-publish-site))

;;; my-org-marsch.el --- Org-publish configuration for klimatmarsch.org

;; Copyright (C) 2025 Jacob Oscarson

;; Author: Jacob Oscarson <jacob@jacoboscarson.se>
;; Keywords: org-mode, publishing, klimatmarsch

;;; Commentary:
;;
;; This file configures org-publish for the klimatmarsch.org website.
;; Features:
;; - Custom export backend for battery icon progress lists
;; - Bootstrap 5.3.3 integration
;; - Swedish text handling ([1/3] → "[1 av 3 klara]")
;; - Automated deployment workflow
;;
;; Battery Icon Mapping:
;;   [ ] → kritiskt.png (critical, not started)
;;   [-] → jobbar.png (in progress)
;;   [X] → snart.png (almost complete)
;;
;; Usage:
;;   M-x marsch-publish-site    ; Build and open in browser
;;   M-x marsch-deploy-site     ; Deploy to server
;;   C-c m p / C-c m d / C-c m s / C-c m c
;;
;; For personal site adaptation, search for "ADAPTATION POINT" comments.

;;; Code:

(require 'ox-html)
(require 'ox-publish)

;;; ============================================================
;;; SECTION 1: Custom Export Backend for Battery Icon Lists
;;; ============================================================

(defconst marsch-battery-icon-alist
  '((off   . "/img/1-kritiskt.png")
    (trans . "/img/3-jobbar.png")
    (on    . "/img/4-snart.png"))
  "Mapping of checkbox states to battery icon paths.
States:
  off   - [ ] unchecked → kritiskt.png (critical)
  trans - [-] in progress → jobbar.png (working)
  on    - [X] checked → snart.png (almost done)")

(defconst marsch-battery-icon-alt-text-alist
  '((off   . "Kritiskt")
    (trans . "Pågår")
    (on    . "Snart klart"))
  "Swedish alt text for battery icons.")

(defun marsch--strip-yasnippet-glyphs (text)
  "Remove yasnippet editing glyphs from TEXT.
Strips patterns like: # 🔶, # ✅, # ⬜"
  (when text
    (replace-regexp-in-string
     "[ \t]*#[ \t]*[🔶✅⬜🟢🟡🔴⚪][ \t\n\r]*"
     ""
     text)))

(defun marsch--format-swedish-stats (total checked)
  "Format checkbox statistics as Swedish text.
Example: (3 1) → '[1 av 3 klara]'"
  (format "[%d av %d klara]" checked total))

(defun marsch--extract-and-convert-stats (text)
  "Extract [X/Y] statistics from TEXT and convert to Swedish.
Returns (CONVERTED-TEXT . HAS-STATS).
Example: 'Foo [1/3]' → ('Foo' . '[1 av 3 klara]')"
  (when text
    (if (string-match "\\[\\([0-9]+\\)/\\([0-9]+\\)\\]" text)
        (let* ((checked (string-to-number (match-string 1 text)))
               (total (string-to-number (match-string 2 text)))
               (swedish-stats (marsch--format-swedish-stats total checked))
               (text-without-stats (replace-regexp-in-string
                                    "[ \t]*\\[\\([0-9]+\\)/\\([0-9]+\\)\\][ \t]*"
                                    ""
                                    text)))
          (cons text-without-stats swedish-stats))
      (cons text nil))))

(defun marsch--count-checkboxes (item)
  "Count checkbox statistics for ITEM and its children.
Returns (TOTAL CHECKED) as a list."
  (let ((total 0)
        (checked 0))
    (org-element-map (org-element-contents item) 'item
      (lambda (child-item)
        (let ((checkbox (org-element-property :checkbox child-item)))
          (when checkbox
            (setq total (1+ total))
            (when (eq checkbox 'on)
              (setq checked (1+ checked))))))
      nil nil 'item)
    (list total checked)))

(defun marsch-html-item (item contents info)
  "Transcode ITEM element to HTML with battery icon rendering.
CONTENTS is the already-exported HTML contents of the item. INFO is a plist
holding contextual information.

This function:
- Renders [ ] as kritiskt.png (critical)
- Renders [-] as jobbar.png (in progress)
- Renders [X] as snart.png (almost done)
- Converts [1/3] to '[1 av 3 klara]' (Swedish)
- Strips yasnippet glyphs (# 🔶, # ✅, # ⬜)"
  (let* ((checkbox (org-element-property :checkbox item))
         (counter (org-element-property :counter item))
         (tag (org-element-property :tag item))

         ;; CONTENTS already has emphasis markup properly rendered to HTML
         ;; Strip paragraph tags - they wrap list item content by default
         (html-no-p
          (when contents
            ;; Remove <p>...</p> wrapper if present
            (let ((trimmed (string-trim contents)))
              (if (string-match "\\`<p>\\(.*\\)</p>\\'" trimmed)
                  (match-string 1 trimmed)
                contents))))

         ;; Strip yasnippet glyphs from HTML
         (html-no-glyphs (marsch--strip-yasnippet-glyphs (or html-no-p "")))

         ;; Extract and convert statistics [1/3] → [1 av 3 klara] in HTML
         (stats-result (marsch--extract-and-convert-stats html-no-glyphs))
         (item-text-clean (car stats-result))
         (swedish-stats (cdr stats-result))

         ;; Battery icon HTML
         (battery-html
          (when checkbox
            (let ((icon-path (cdr (assq checkbox marsch-battery-icon-alist)))
                  (alt-text (cdr (assq checkbox marsch-battery-icon-alt-text-alist))))
              (format "<img src=\"%s\" alt=\"%s\" class=\"task-status\"> "
                      icon-path alt-text))))

         ;; Statistics HTML (Swedish format)
         (stats-html
          (when swedish-stats
            (format "<code>%s</code> " swedish-stats))))

    ;; Build complete list item HTML
    (concat
     "<li class=\"marsch-item\">"
     (or battery-html "")
     ;; Add tag as heading if present (for definition lists)
     (when tag
       (format "<h3>%s</h3>\n" (org-export-data tag info)))
     (or stats-html "")
     (or item-text-clean "")
     "</li>\n")))

(defun marsch-html-plain-list (plain-list contents info)
  "Transcode PLAIN-LIST element to HTML with custom class.
CONTENTS is the contents of the list. INFO is a plist holding
contextual information."
  (let* ((type (org-element-property :type plain-list))
         (list-tag (if (eq type 'ordered) "ol" "ul"))
         (attributes (org-export-read-attribute :attr_html plain-list))
         (class (or (plist-get attributes :class) "marsch-list")))
    (format "<%s class=\"%s\">\n%s</%s>\n"
            list-tag
            class
            (or contents "")
            list-tag)))

(defun marsch-html-headline (headline contents info)
  "Transcode HEADLINE element to HTML with Swedish statistics and ATTR_HTML id.
CONTENTS is the contents of the headline. INFO is a plist holding
contextual information.

Converts [1/3] statistics to Swedish '[1 av 3 klara]' format.
Also injects id from #+ATTR_HTML: :id into the wrapper div."
  ;; Get the standard HTML output first
  (let* ((html (org-html-headline headline contents info))
         ;; Extract headline text to check for statistics
         (text (org-export-data (org-element-property :title headline) info))
         (stats-result (marsch--extract-and-convert-stats text))
         ;; Get ATTR_HTML properties for id injection
         (attr-html (org-element-property :ATTR_HTML headline))
         (custom-id (when attr-html (plist-get (car attr-html) :id)))
         (custom-class (when attr-html (plist-get (car attr-html) :class))))

    ;; First handle statistics conversion
    (setq html
          (if (cdr stats-result)
              ;; If we have statistics, replace [X/Y] with Swedish
              (let ((swedish-stats (cdr stats-result)))
                (replace-regexp-in-string
                 "<code>\\[\\([0-9]+\\)/\\([0-9]+\\)\\]</code>"
                 (lambda (_match)
                   (format "<code>%s</code>" swedish-stats))
                 html))
            html))

    ;; Then inject custom id if present (using org-export-read-attribute)
    (let* ((attr (org-export-read-attribute :attr_html headline))
           (id (plist-get attr :id))
           (class (plist-get attr :class)))
      (if id
          (replace-regexp-in-string
           "id=\"outline-container-org[a-z0-9]+\""
           (format "id=\"%s\"" id)
           html)
        html))))

;; Custom transcoder for quote blocks -> aside with notice styling
(defun marsch-html-quote-block (quote-block contents info)
  "Transcode QUOTE-BLOCK to HTML aside element for notice boxes."
  (let* ((attr (org-export-read-attribute :attr_html quote-block))
         (class (plist-get attr :class)))
    (if class
        (format "<aside class=\"%s\">\n  <div class=\"notice-icon\" aria-hidden=\"true\">ℹ️</div>\n  <div class=\"notice-content\">\n%s  </div>\n</aside>"
                class
                contents)
      ;; Default blockquote if no class specified
      (org-html-quote-block quote-block contents info))))

;; Define the custom export backend
(org-export-define-derived-backend 'marsch-html 'html
  :translate-alist '((headline . marsch-html-headline)
                     (item . marsch-html-item)
                     (plain-list . marsch-html-plain-list)
                     (quote-block . marsch-html-quote-block)))

;;; ============================================================
;;; SECTION 2: HTML Head Template with Bootstrap
;;; ============================================================

(defconst marsch-html-head-template
  "<meta charset=\"UTF-8\">
<meta name=\"viewport\" content=\"width=device-width, initial-scale=1.0\">
<meta name=\"description\" content=\"Stor klimatmarsch i Göteborg söndagen den 6 september 2026, en vecka före valet\">
<meta name=\"keywords\" content=\"klimatmarsch, klimat, Göteborg, demonstration, val 2026\">
<link rel=\"icon\" type=\"image/x-icon\" href=\"/favicon.ico\">
<link rel=\"stylesheet\" href=\"/css/bootstrap.min.css\">
<link rel=\"stylesheet\" href=\"/css/style.css?v=9\">
<style>
/* Battery icon progress list styling */
.task-status {
  width: 20px;
  height: 20px;
  vertical-align: middle;
  margin-right: 0.5rem;
  display: inline-block;
}

.marsch-list {
  list-style: none;
  padding-left: 0;
  line-height: 1.8;
}

.marsch-item {
  margin-bottom: 0.5rem;
}

.marsch-stats {
  color: #007bff;
  font-weight: 600;
  font-size: 0.9em;
  margin-left: 0.25rem;
}

/* Nested list indentation */
.marsch-list .marsch-list {
  padding-left: 2rem;
  margin-top: 0.25rem;
}
</style>
<script src=\"https://cdn.jsdelivr.net/npm/bootstrap@5.3.3/dist/js/bootstrap.bundle.min.js\" integrity=\"sha384-YvpcrYf0tY3lHB60NNkmXc5s9fDVZLESaAA55NDzOxhy9GkcIdslK1eN7N6jIeHz\" crossorigin=\"anonymous\"></script>"
  "HTML head template for klimatmarsch.org with Bootstrap and battery icons.

ADAPTATION POINT: For personal site, modify:
- Meta description and keywords
- Color scheme for .marsch-stats
- Add additional custom styles")

;;; ============================================================
;;; SECTION 3: Org-Publish Project Configuration
;;; ============================================================

(defconst marsch-base-directory
  "/Users/jacob/src/mine/skunkworks/docs/politik/xr/val2026/hemsida"
  "Base directory for klimatmarsch.org source files.

ADAPTATION POINT: For personal site, change to personal site directory.")

(defconst marsch-publishing-directory
  "/Users/jacob/src/mine/skunkworks/docs/politik/xr/val2026/hemsida/gen"
  "Publishing directory for klimatmarsch.org generated files.

ADAPTATION POINT: For personal site, change to personal site gen/ directory.")

(defun marsch--load-preamble ()
  "Load HTML preamble from fragment/toppen.html file.
This keeps the preamble HTML in a separate file for easier editing."
  (let ((preamble-file
         (expand-file-name "fragment/toppen.html" marsch-base-directory)))
    (if (file-exists-p preamble-file)
        (with-temp-buffer
          (insert-file-contents preamble-file)
          (buffer-string))
      (error "Preamble file not found: %s" preamble-file))))

(defun marsch--load-postamble ()
  "Load HTML postamble from fragment/botten.html file.
This keeps the postamble HTML in a separate file for easier editing."
  (let ((postamble-file
         (expand-file-name "fragment/botten.html" marsch-base-directory)))
    (if (file-exists-p postamble-file)
        (with-temp-buffer
          (insert-file-contents postamble-file)
          (buffer-string))
      (error "Postamble file not found: %s" postamble-file))))

(setq org-publish-project-alist
      `(
        ;; ─────────────────────────────────────────────────────────
        ;; Content project: Org files → HTML
        ;; ─────────────────────────────────────────────────────────
        ("marsch-content"
         :base-directory ,marsch-base-directory
         :base-extension "org"
         :publishing-directory ,marsch-publishing-directory
         :recursive t
         :exclude "gen/"
         :publishing-function org-marsch-html-publish-to-html

         ;; HTML export settings
         :html-doctype "html5"
         :html-html5-fancy t
         :html-head-include-default-style nil
         :html-head-include-scripts nil
         :html-head ,marsch-html-head-template
         :html-validation-link nil
         :html-preamble ,(marsch--load-preamble)
         :html-postamble ,(marsch--load-postamble)

         ;; Content settings
         :with-toc nil
         :with-title nil
         :with-author nil
         :with-date nil
         :with-timestamps nil
         :section-numbers nil

         ;; Language
         :language "sv")

        ;; ─────────────────────────────────────────────────────────
        ;; Static files project: Copy assets
        ;; ─────────────────────────────────────────────────────────
        ("marsch-static"
         :base-directory ,marsch-base-directory
         :base-extension "css\\|png\\|jpg\\|jpeg\\|gif\\|ico\\|js\\|txt\\|pdf"
         :publishing-directory ,marsch-publishing-directory
         :recursive t
         :exclude "gen/"
         :publishing-function org-publish-attachment)

        ;; ─────────────────────────────────────────────────────────
        ;; Combined project: Run both
        ;; ─────────────────────────────────────────────────────────
        ("marsch-site"
         :components ("marsch-content" "marsch-static"))))

;;; ============================================================
;;; SECTION 4: Custom Publishing Function
;;; ============================================================

(defun org-marsch-html-publish-to-html (plist filename pub-dir)
  "Publish an org file to HTML using the marsch-html backend.

PLIST is the property list for the given project.
FILENAME is the filename of the Org file to be published.
PUB-DIR is the publishing directory."
  (org-publish-org-to 'marsch-html filename
                      (concat "." (or (plist-get plist :html-extension)
                                      org-html-extension
                                      "html"))
                      plist pub-dir))

;;; ============================================================
;;; SECTION 5: Interactive Commands
;;; ============================================================

(defun marsch-publish-site (&optional force)
  "Publish the klimatmarsch.org site.

With prefix argument FORCE, rebuild all files regardless of timestamps."
  (interactive "P")
  (message "Publishing klimatmarsch.org site%s..."
           (if force " (forcing rebuild)" ""))

  ;; Clear cache if forcing rebuild
  (when force
    (org-publish-remove-all-timestamps))

  ;; Publish the site
  (condition-case err
      (progn
        (org-publish-project "marsch-site" force)
        (message "✓ klimatmarsch.org published successfully!")

        ;; Ask to open in browser
        (when (y-or-n-p "Open index.html in browser? ")
          (browse-url "http://localhost:8081/")))
    (error
     (message "✗ Publishing failed: %s" (error-message-string err))
     (error "Publishing failed: %s" (error-message-string err)))))

(defun marsch-deploy-site ()
  "Deploy the klimatmarsch.org site to the server via Makefile."
  (interactive)
  (message "Deploying klimatmarsch.org to server...")

  (let ((default-directory marsch-base-directory))
    (condition-case err
        (progn
          ;; Check if Makefile exists
          (unless (file-exists-p (expand-file-name "Makefile" marsch-base-directory))
            (error "Makefile not found in %s" marsch-base-directory))

          ;; Run make deploy
          (let ((output (shell-command-to-string "make deploy")))
            (message "Deployment output:\n%s" output)
            (message "✓ klimatmarsch.org deployed successfully!")))
      (error
       (message "✗ Deployment failed: %s" (error-message-string err))
       (error "Deployment failed: %s" (error-message-string err))))))

(defun marsch-publish-and-deploy (&optional force)
  "Publish and deploy the klimatmarsch.org site.

With prefix argument FORCE, rebuild all files regardless of timestamps."
  (interactive "P")
  (marsch-publish-site force)
  (when (y-or-n-p "Deploy to server? ")
    (marsch-deploy-site)))

(defun marsch-clear-cache ()
  "Clear the org-publish timestamp cache for klimatmarsch.org."
  (interactive)
  (when (y-or-n-p "Clear org-publish timestamp cache? ")
    (org-publish-remove-all-timestamps)
    (message "✓ Timestamp cache cleared")))

;;; ============================================================
;;; SECTION X: Pax log deterministic headers
;;; ============================================================

(defun pax/heading-3 () (interactive) (newline) (save-excursion (insert "*** ")))
(defun pax/heading-4 () (interactive) (newline) (save-excursion (insert "**** ")))
(defun pax/heading-5 () (interactive) (newline) (save-excursion (insert "***** ")))
(defun pax/heading-6 () (interactive) (newline) (save-excursion (insert "****** ")))

;;; ============================================================
;;; SECTION 6: Keybindings
;;; ============================================================

(with-eval-after-load 'org
  (define-key org-mode-map (kbd "C-c 3") #'pax/heading-3)
  (define-key org-mode-map (kbd "C-c 4") #'pax/heading-4)
  (define-key org-mode-map (kbd "C-c 5") #'pax/heading-5)
  (define-key org-mode-map (kbd "C-c 6") #'pax/heading-6)
  (define-key org-mode-map (kbd "C-c m p") #'marsch-publish-site)
  (define-key org-mode-map (kbd "C-c m d") #'marsch-deploy-site)
  (define-key org-mode-map (kbd "C-c m s") #'marsch-publish-and-deploy)
  (define-key org-mode-map (kbd "C-c m c") #'marsch-clear-cache))

;;; ============================================================
;;; SECTION 7: Load Confirmation
;;; ============================================================

(message "✓ my-org-marsch.el loaded successfully")
(message "  Commands available:")
(message "    M-x marsch-publish-site         - Build site")
(message "    M-x marsch-deploy-site          - Deploy to server")
(message "    M-x marsch-publish-and-deploy   - Build and deploy")
(message "    M-x marsch-clear-cache          - Clear timestamp cache")
(message "  Keybindings (in org-mode):")
(message "    C-c m p  - Publish site")
(message "    C-c m d  - Deploy site")
(message "    C-c m s  - Publish and deploy")
(message "    C-c m c  - Clear cache")


(provide 'pax)

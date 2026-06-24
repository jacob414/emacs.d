;;; my-option-frame.el --- Small non-intrusive option picker -*- lexical-binding: t; -*-

;;; Commentary:
;; `my/choose-from-frame' asks for one value from a short list.  It uses
;; posframe in graphical sessions when available, and falls back to
;; `completing-read' everywhere else.

;;; Code:

(require 'cl-lib)

(declare-function posframe-hide "posframe")
(declare-function posframe-poshandler-point-bottom-left-corner "posframe")
(declare-function posframe-show "posframe")

(defgroup my-option-frame nil
  "Small option picker displayed in a popup frame."
  :group 'convenience)

(defcustom my-option-frame-keys
  "123456789abcdefghijklmnopqrstuvwxyz"
  "Keys assigned to options shown by `my/choose-from-frame'."
  :type 'string
  :group 'my-option-frame)

(defun my-option-frame--choices (options)
  "Return normalized choice plists for OPTIONS.

Each OPTIONS entry may be either (LABEL . VALUE), in which case
LABEL is displayed and VALUE is returned, or a plain value, in
which case the displayed label is `(format \"%s\" VALUE)' and the
plain value is returned."
  (unless options
    (user-error "No options supplied"))
  (when (> (length options) (length my-option-frame-keys))
    (user-error "Too many options supplied; maximum is %d"
                (length my-option-frame-keys)))
  (cl-mapcar
   (lambda (key option)
     (let* ((label (if (consp option) (car option) option))
            (value (if (consp option) (cdr option) option)))
       (list :key key :label (format "%s" label) :value value)))
   (string-to-list my-option-frame-keys)
   options))

(defun my-option-frame--value-for-key (choices key)
  "Return the value in CHOICES selected by KEY, or nil."
  (when-let ((choice (cl-find key choices :key (lambda (choice)
                                                (plist-get choice :key)))))
    (plist-get choice :value)))

(defun my-option-frame--buffer-text (prompt choices)
  "Return display text for PROMPT and CHOICES."
  (concat
   (propertize prompt 'face 'minibuffer-prompt)
   "\n"
   (mapconcat
    (lambda (choice)
      (format "[%c] %s" (plist-get choice :key) (plist-get choice :label)))
    choices
    "\n")))

(defun my-option-frame--read-with-completing-read (prompt choices)
  "Read one of CHOICES with PROMPT using `completing-read'."
  (let* ((labels (mapcar (lambda (choice) (plist-get choice :label)) choices))
         (selected (completing-read prompt labels nil t)))
    (unless (string-empty-p selected)
      (plist-get (cl-find selected choices
                          :key (lambda (choice) (plist-get choice :label))
                          :test #'string=)
                 :value))))

(defun my-option-frame--read-with-posframe (prompt choices)
  "Read one of CHOICES with PROMPT using a posframe."
  (require 'posframe)
  (let ((buffer (get-buffer-create " *my option frame*"))
        (selected nil)
        (done nil))
    (with-current-buffer buffer
      (setq buffer-read-only nil)
      (erase-buffer)
      (insert (my-option-frame--buffer-text prompt choices))
      (goto-char (point-min))
      (setq buffer-read-only t))
    (unwind-protect
        (progn
          (posframe-show
           buffer
           :position (point)
           :poshandler #'posframe-poshandler-point-bottom-left-corner
           :border-width 1
           :border-color "gray50"
           :internal-border-width 1)
          (while (not done)
            (let ((key (read-key)))
              (cond
               ((memq key '(?\e))
                (setq done t
                      selected nil))
               ((setq selected (my-option-frame--value-for-key choices key))
                (setq done t)))))
          selected)
      (posframe-hide buffer))))

;;;###autoload
(defun my/choose-from-frame (options &optional prompt)
  "Ask for one value from OPTIONS and return it.

OPTIONS should be a short list.  Each element can be either a
plain value or a (LABEL . VALUE) cons.  The selected VALUE is
returned, so the result can be passed directly as an argument to
another function.

When `posframe' is available in a graphical session, choices are
shown in a small child frame near point.  Otherwise this falls
back to `completing-read'.  `C-g' always quits; ESC cancels the
posframe picker and returns nil."
  (let ((choices (my-option-frame--choices options))
        (prompt (or prompt "Choose: ")))
    (if (and (display-graphic-p)
             (require 'posframe nil 'noerror))
        (my-option-frame--read-with-posframe prompt choices)
      (my-option-frame--read-with-completing-read prompt choices))))

(provide 'my-option-frame)

;;; my-option-frame.el ends here

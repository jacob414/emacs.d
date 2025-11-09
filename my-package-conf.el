(require 'package)
(setq package-user-dir (expand-file-name "~/src/mine/elpa.d"))
(setq package-archives '(("gnu"   . "https://elpa.gnu.org/packages/")
                         ("melpa" . "https://melpa.org/packages/")))
(unless (bound-and-true-p package--initialized)
  (package-initialize))

(provide 'my-package-conf)

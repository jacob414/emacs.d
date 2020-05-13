(require 'package)
(setq package-user-dir (concat "~/src/mine/elpa.d"))
(setq my-package-sources '(("melpa" . "https://elpa.zilongshanren.com/melpa/")
                           ("marmalade" . "http://marmalade-repo.org/packages/")
                           ("elpa" . "http://tromey.com/elpa/") ) )

(add-to-list 'package-archives my-package-sources t)

(package-initialize)

(provide 'my-package-conf)

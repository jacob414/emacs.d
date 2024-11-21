(setq elpy-rpc-python-command "~/opt/plus/conda-plus/bin/python")

(setq langtool-bin "/opt/homebrew/bin/languagetool")

(org2web-add-project
 '("my-website"
   :repository-directory "~/src/mine/site"
   :remote (git "git@github.com:jacob414/site.git" "org2web")
   ;; Alternatively, use rclone:
   ;; :remote (rclone "remote-name" "/remote/path/location")
   :site-domain "https://414soft.com"
   :site-main-title "Jacob 414"
   :site-sub-title "Underrubrik?"
   :theme (worg)
   :source-browse-url ("GitHub" "https://github.com/yourusername/yourrepository")
   :personal-avatar "/media/img/avatar.jpg"
   :personal-duoshuo-shortname "your-duoshuo-shortname"
   :web-server-port 7654))


(provide 'zipfly)

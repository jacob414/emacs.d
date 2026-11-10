;;; dimman.el --- Host config for dimman (Debian file server) -*- lexical-binding: t -*-
;;
;; Mirrors zipfly.el minus the macOS parts (Homebrew paths, ns-* modifiers,
;; LanguageTool binary) and minus the Pax integration, which talks to the
;; tmux session on zipfly.

(require 'server)
(unless (server-running-p)
  (server-start))

(provide 'dimman)

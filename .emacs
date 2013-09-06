(add-to-list 'load-path "/usr/local/go/misc/emacs" t)
(require 'go-mode-load)

(setq backup-directory-alist `(("." . "~/.emacs.d/backups")))
(setq backup-by-copying t)
(setq delete-old-versions t
      kept-new-versions 5
      kept-old-versions 2
      version-control t)
;(require 'whitespace)

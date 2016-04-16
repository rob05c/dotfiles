(require 'cl)

(add-to-list 'load-path "~/.emacs.d/lisp")

;;
;; UI
;;
(menu-bar-mode -1)


;;
;; backups
;;

(setq vc-make-backup-files t)
(setq backup-directory-alist `(("." . "~/.emacs.d/backups")))
(setq backup-by-copying t)
(setq delete-old-versions t
      kept-new-versions 20
      kept-old-versions 2
      version-control t)


;;
;; modes
;;

(add-to-list 'auto-mode-alist '("\\.cu\\'" . c++-mode))

(autoload 'glsl-mode "glsl-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.frag\\'" . glsl-mode))
(add-to-list 'auto-mode-alist '("\\.vert\\'" . glsl-mode))
(add-to-list 'auto-mode-alist '("\\.vs\\'" . glsl-mode))
(add-to-list 'auto-mode-alist '("\\.fs\\'" . glsl-mode))

(autoload 'markdown-mode "markdown-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))

(autoload 'nimrod-mode "nimrod-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.nim\\'" . nimrod-mode))

(autoload 'web-mode "web-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.html\\'" . web-mode))

(autoload 'go-mode "go-mode" "Go Mode." t)
(add-to-list 'auto-mode-alist '("\\.go\\'" . go-mode))

(autoload 'dockerfile-mode "dockerfile-mode" "Dockerfile Mode." t)
(add-to-list 'auto-mode-alist '("\\Dockerfile\\'" . dockerfile-mode))

(autoload 'web-mode "web-mode" "Web Mode." t)
(add-to-list 'auto-mode-alist '("\\.html\\'" . web-mode))

(autoload 'arc-mode "arc-mode" "Arc Mode." t)
(add-to-list 'auto-mode-alist '("\\.arc\\'" . arc-mode))


;;
;; code formatting
;;

(setq c-default-style "bsd"
			c-basic-offset 2)
(setq compile-command "make -B")
(global-set-key (kbd "C-x O") 'previous-multiframe-window)
(c-set-offset 'innamespace 0)
(setq indent-namespaces "no")
(setq gdb-many-windows 1)
(setq default-tab-width 2)
(global-visual-line-mode 1)
;; disabling autofill is a pain, so we set the fill column huge instead
(setq-default fill-column 10000000)

(setq js-indent-level 2)
(add-hook 'html-mode-hook
	  (lambda ()
	    ;; Default indentation is usually 2 spaces, changing to 4.
	    (set (make-local-variable 'sgml-basic-offset) 2)))

(defun my-go-mode-hook ()
	;; Call Gofmt before saving
	(add-hook 'before-save-hook 'gofmt-before-save)
	;; Customize compile command to run go build
	(if (not (string-match "go" compile-command))
			(set (make-local-variable 'compile-command)
					 ;; "go build -v && go test -v && go vet"))
					 "go build -v"))
	;; Godef jump key binding
	(local-set-key (kbd "M-.") 'godef-jump))
(add-hook 'go-mode-hook 'my-go-mode-hook)

;; save-place (saves last cursor position in each file
(setq save-place-file "~/.emacs.d/saveplace")
(setq-default save-place t)
(require 'saveplace)

(load-theme 'manoj-dark)
(set-face-foreground 'mode-line "black")
(set-face-background 'mode-line "gold")
(set-face-background 'mode-line-inactive "yellow")
(set-face-foreground 'mode-line-inactive "black")

(setq inhibit-startup-screen +1)
(setq initial-scratch-message "")

(setq-default show-trailing-whitespace +1)


;;
;; tabs
;;
(custom-set-faces
 '(my-tab-face ((((class color)) (:foreground "brightblack"))) t)
 )
;; add custom font locks to all buffers and all files
(add-hook
 'font-lock-mode-hook
 (function
	(lambda ()
		(setq
		 font-lock-keywords
		 (append
			font-lock-keywords
			'(
				("\t" (0 'my-tab-face t))
				))))))

(standard-display-ascii ?\t "» ")
;; (setq-default indent-tabs-mode nil)

;; gofmt means I don't care if there are tabs or spaces while editing
(add-hook 'go-mode-hook (lambda () (standard-display-ascii ?\t "\t")))


;;
;; tools
;;
(defun increment-number-at-point ()
	(interactive)
	(skip-chars-backward "0123456789")
	(or (looking-at "[0123456789]+")
			(error "No number at point"))
	(replace-match (number-to-string (1+ (string-to-number (match-string 0))))))
(global-set-key (kbd "C-c +") 'increment-number-at-point)

; (autoload 'org-mode "arc-mode" "Arc Mode." t)
;; (require 'org)
;; (define-key global-map "\C-cl" 'org-store-link)
;; (define-key global-map "\C-ca" 'org-agenda)
;; (setq org-log-done t)

(autoload 'string-inflection-cycle "string-inflection" "String Inflection." t)
(global-set-key (kbd "C-c i") 'string-inflection-cycle)


;;;
;;; ido (smart autocomplete of commands/files/buffers)
;;;
(autoload 'smex "smex" "Smex." t)
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)
(global-set-key (kbd "C-c C-c M-x") 'execute-extended-command)
(ido-mode 1)
(ido-everywhere 1)


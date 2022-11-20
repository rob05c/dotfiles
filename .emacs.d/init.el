
;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.

;; (require 'package)
;; (add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
;; (package-initialize)


;; package stuff
(require 'package)

(add-to-list 'package-archives
             '("elpy" . "http://jorgenschaefer.github.io/packages/"))

(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/"))

(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)

(add-to-list 'package-archives
             '("melpa-stable" . "http://melpa-stable.milkbox.net/packages/") t)

(add-to-list 'load-path "~/.emacs.d/site-lisp/")


; list the packages you want
(setq package-list-original
 '(
   python-environment
   deferred
   epc
   flycheck
	 ctable
	 jedi
	 concurrent
	 company
	 cyberpunk-theme
	 elpy
	 yasnippet
	 pyvenv
	 highlight-indentation
	 find-file-in-project
   sql-indent
	 sql
	 exec-path-from-shell
	 iedit
   auto-complete
	 popup
	 let-alist
	 magit
	 git-rebase-mode
   git-commit-mode
	 minimap
	 popup
	 ))

(setq package-list
 '(
  go-mode
  clang-format
	leuven-theme
	afternoon-theme
	;; solarized-theme
	;; smart-mode-line
	;; smart-mode-line-powerline-theme
	telephone-line
  ))


; activate all the packages
(package-initialize)

; fetch the list of packages available
(unless package-archive-contents
  (package-refresh-contents))

; install the missing packages
(dolist (package package-list)
  (unless (package-installed-p package)
    (package-install package)))


;; (load "~/.emacs.d/init-packages")

;; to reinstall packages on a new system:
;; M-x package-install-selected-packages

;; (require 'cl-lib)

(add-to-list 'load-path "~/.emacs.d/lisp")
(add-to-list 'load-path "~/.emacs.d/lisp/neotree")
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes")

(require 'neotree)
(global-set-key (kbd "C-c n") 'neotree-toggle)

(require 'multi-term)
(setq multi-term-program "/bin/bash")

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
;; tabs
;;
;; (custom-set-faces
;;  ;; custom-set-faces was added by Custom.
;;  ;; If you edit it by hand, you could mess it up, so be careful.
;;  ;; Your init file should contain only one such instance.
;;  ;; If there is more than one, they won't work right.
;;  '(my-tab-face ((((class color)) (:foreground "#333338"))) t))

 ;; '(my-tab-face-light ((((class color)) (:foreground "#ccccc6"))) t))
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

(autoload 'web-mode "web-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.html\\'" . web-mode))

(autoload 'elixir-mode "elixir-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.ex\\'" . elixir-mode))
(add-to-list 'auto-mode-alist '("\\.exs\\'" . elixir-mode))

(autoload 'json-mode "json-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.json\\'" . json-mode))

(defun my-web-mode-hook ()
	"Hooks for Web mode."
	(setq web-mode-markup-indent-offset 2)
	(setq web-mode-css-indent-offset 2)
	(setq web-mode-code-indent-offset 2)
	(setq web-mode-indent-style 2)
	)
(add-hook 'web-mode-hook  'my-web-mode-hook)

(autoload 'go-mode "go-mode" "Go Mode." t)
(add-to-list 'auto-mode-alist '("\\.go\\'" . go-mode))

(autoload 'nim-mode "nim-mode" "Nim Mode." t)
(add-to-list 'auto-mode-alist '("\\.nim\\'" . nim-mode))
;; (add-hook 'nim-mode-hook 'nimsuggest-mode)

(autoload 'rust-mode "rust-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.rs\\'" . rust-mode))

(autoload 'dockerfile-mode "dockerfile-mode" "Dockerfile Mode." t)
(add-to-list 'auto-mode-alist '("\\Dockerfile\\'" . dockerfile-mode))

(autoload 'web-mode "web-mode" "Web Mode." t)
(add-to-list 'auto-mode-alist '("\\.html\\'" . web-mode))

(autoload 'arc-mode "arc" "Arc Mode." t)
(add-to-list 'auto-mode-alist '("\\.arc\\'" . arc-mode))

(add-to-list 'auto-mode-alist '("\\.rkt\\'" . scheme-mode))

(autoload 'swift-mode "swift-mode" "Swift Mode." t)
(add-to-list 'auto-mode-alist '("\\.swift\\'" . swift-mode))

(add-hook 'swift-mode-hook
	(lambda ()
		(set indent-tabs-mode nil)))

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
(setq-default tab-width 2)

;; (global-visual-line-mode 1)
(setq line-move-visual nil)

;; disabling autofill is a pain, so we set the fill column huge instead
(setq-default fill-column 10000000)

(setq js-indent-level 2)
(add-hook 'html-mode-hook
	  (lambda ()
	    (set (make-local-variable 'sgml-basic-offset) 2)))

(add-hook 'js-mode-hook
	(lambda ()
		(set (make-local-variable 'sgml-basic-offset) 2)))

(add-hook 'go-mode-hook
					(lambda ()
						;; (require 'go-dlv)
						;; Call Gofmt before saving
						(add-hook 'before-save-hook 'gofmt-before-save)
						;; Customize compile command to run go build
						(if (not (string-match "go" compile-command))
								(set (make-local-variable 'compile-command)
										 ;; "go build -v && go test -v && go vet"))
										 "go build -v"))
						;; Godef jump key binding
						(local-set-key (kbd "M-.") 'godef-jump)))

(defun clang-format-save-hook-for-this-buffer ()
  "Create a buffer local save hook."
  (add-hook 'before-save-hook
            (lambda ()
              (when (locate-dominating-file "." ".clang-format")
                (clang-format-buffer))
              ;; Continue to save.
              nil)
            nil
            ;; Buffer local hook.
            t))

;; Run this for each mode you want to use the hook.
(add-hook 'c-mode-hook (lambda () (clang-format-save-hook-for-this-buffer)))
(add-hook 'c++-mode-hook (lambda () (clang-format-save-hook-for-this-buffer)))
(add-hook 'glsl-mode-hook (lambda () (clang-format-save-hook-for-this-buffer)))

(setq rust-format-on-save t)
(add-hook 'rust-mode-hook
	(lambda () (setq compile-command "cargo build")))

;; save-place (saves last cursor position in each file
(setq save-place-file "~/.emacs.d/saveplace")
;(setq-default save-place t)
(require 'saveplace)
(save-place-mode 1)

(menu-bar-mode -1)

(setq inhibit-startup-screen +1)
(setq initial-scratch-message "")

(setq-default show-trailing-whitespace +1)

;; gofmt means I don't care if there are tabs or spaces while editing
;; (add-hook 'go-mode-hook (lambda () (standard-display-ascii ?\t "\t")))

(add-hook 'before-save-hook #'gofmt-before-save)

(defun my-elixir-hook ()
	(setq indent-tabs-mode nil)
	(setq tab-width 2))
(add-hook 'elixir-mode-hook 'my-elixir-hook)

(add-hook 'elixir-mode-hook
          (lambda () (add-hook 'before-save-hook 'elixir-format nil t)))

(defun my-web-hook ()
	(setq indent-tabs-mode nil)
	(setq tab-width 2))
(add-hook 'web-mode-hook 'my-web-hook)

(defun my-js-hook ()
	(setq indent-tabs-mode nil)
	(setq tab-width 2))
(add-hook 'js-mode-hook 'my-js-hook)

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

(global-prettify-symbols-mode +1)
(defun my-add-pretty-lambda ()
  "make some word or string show as pretty Unicode symbols"
	(setq prettify-symbols-alist
				'(
					("lambda" . 955) ; λ
					("func" . 955)   ; λ
					;; ("struct" . 963)   ; σ
					("->" . 8594)    ; →
					("=>" . 8658)    ; ⇒
					(">=" . 8805)    ; ≥
					("<=" . 8804)    ; ≤
					("!=" . 8800)    ; ≠
					;; (":=" . 8592)    ; ←

					(":=" . 8788)    ; ≔

					;; ("==" . 65309)   ; ＝
					;; ("<-" . 8695)    ; ⇷
					("<-" . 8592)    ; ←
					;; ("map" . 8614)    ; ↦
					("&&" . 8743)    ; ∧
					("||" . 8744)    ; ∨
					(" * " . 183)    ; ·
					;; ("util" . 128295)    ; 🔧
					))
	)
(add-hook 'go-mode-hook 'my-add-pretty-lambda)


;; horizontal scrolling
;; (if (boundp 'truncate-lines)
;;     (setq-default truncate-lines t) ; always truncate
;;   (progn
;;     (hscroll-global-mode t)
;;     (setq hscroll-margin 1)
;;     (setq auto-hscroll-mode 1)
;;     (setq automatic-hscrolling t)
;; 		))
;; (put 'scroll-left 'disabled nil)
;; (put 'scroll-right 'disabled nil)
;; (global-set-key (kbd "C-M-f") 'scroll-left)
;; (global-set-key (kbd "C-M-b") 'scroll-right)


;; (defun my-js-mode-hook ()
;; 	(add-hook 'before-save-hook 'jsfmt-before-save))
;; (add-hook 'js-mode-hook 'my-js-mode-hook)

;; (defun jsfmt-before-save ()
;; 	(jsfmt))

;; (defcustom jsfmt-command "closure-compiler --formatting=PRETTY_PRINT"
;;     "The 'jsfmt' closure-compiler formatting command."
;;     :type 'string
;;     :group 'js)

;; (defun jsfmt ()
;;   "Format the current buffer according to the closure-compiler tool."
;;   (interactive)
;;   (let ((tmpfile (make-temp-file "jsfmt" nil ".js"))
;; 	(patchbuf (get-buffer-create "*Jsfmt patch*"))
;; 	(errbuf (if jsfmt-show-errors (get-buffer-create "*Jsfmt Errors*")))
;; 	(coding-system-for-read 'utf-8)
;; 	(coding-system-for-write 'utf-8))

;;     (save-restriction
;;       (widen)
;;       (if errbuf
;; 	  (with-current-buffer errbuf
;; 	    (setq buffer-read-only nil)
;; 	    (erase-buffer)))
;;       (with-current-buffer patchbuf
;; 	(erase-buffer))

;;       (write-region nil nil tmpfile)

;;       ;; We're using errbuf for the mixed stdout and stderr output. This
;;       ;; is not an issue because closure-compiler --js-output-file does not produce any stdout
;;       ;; output in case of success.
;;       (if (zerop (call-process jsfmt-command nil errbuf nil "--js-output-file" tmpfile))
;; 	  (progn
;; 	    (if (zerop (call-process-region (point-min) (point-max) "diff" nil patchbuf nil "-n" "-" tmpfile))
;; 		(message "Buffer is already formatted")
;; 	      (go--apply-rcs-patch patchbuf)
;; 	      (message "Applied jsfmt"))
;; 	    (if errbuf (jsfmt--kill-error-buffer errbuf)))
;; 	(message "Could not apply jsfmt")
;; 	(if errbuf (jsfmt--process-errors (buffer-file-name) tmpfile errbuf)))

;;       (kill-buffer patchbuf)
;;       (delete-file tmpfile))))

;; (defcustom jsfmt-show-errors 'buffer
;;     "Where to display jsfmt error output.
;; It can either be displayed in its own buffer, in the echo area, or not at all.

;; Please note that Emacs outputs to the echo area when writing
;; files and will overwrite jsfmt's echo output if used from inside
;; a `before-save-hook'."
;;     :type '(choice
;; 	    (const :tag "Own buffer" buffer)
;; 	    (const :tag "Echo area" echo)
;; 	    (const :tag "None" nil))
;;     :group 'js)

;; (defun jsfmt--process-errors (filename tmpfile errbuf)
;;   (with-current-buffer errbuf
;;     (if (eq jsfmt-show-errors 'echo)
;; 	(progn
;; 	  (message "%s" (buffer-string))
;; 	  (jsfmt--kill-error-buffer errbuf))
;;       ;; Convert the jsfmt stderr to something understood by the compilation mode.
;;       (goto-char (point-min))
;;       (insert "jsfmt errors:\n")
;;       (while (search-forward-regexp (concat "^\\(" (regexp-quote tmpfile) "\\):") nil t)
;; 	(replace-match (file-name-nondirectory filename) t t nil 1))
;;       (compilation-mode)
;;       (display-buffer errbuf))))

;; (defun jsfmt--kill-error-buffer (errbuf)
;;   (let ((win (get-buffer-window errbuf)))
;;     (if win
;; 	(quit-window t win)
;;       (kill-buffer errbuf))))

;; (defun js--goto-line (line)
;;   (goto-char (point-min))
;;   (forward-line (1- line)))

;; (defun js--delete-whole-line (&optional arg)
;;     "Delete the current line without putting it in the `kill-ring'.
;; Derived from function `kill-whole-line'.  ARG is defined as for that
;; function."
;;     (setq arg (or arg 1))
;;     (if (and (> arg 0)
;; 	     (eobp)
;; 	     (save-excursion (forward-visible-line 0) (eobp)))
;; 	(signal 'end-of-buffer nil))
;;     (if (and (< arg 0)
;; 	     (bobp)
;; 	     (save-excursion (end-of-visible-line) (bobp)))
;; 	(signal 'beginning-of-buffer nil))
;;     (cond ((zerop arg)
;; 	   (delete-region (progn (forward-visible-line 0) (point))
;; 			  (progn (end-of-visible-line) (point))))
;; 	  ((< arg 0)
;; 	   (delete-region (progn (end-of-visible-line) (point))
;; 			  (progn (forward-visible-line (1+ arg))
;; 				 (unless (bobp)
;; 				   (backward-char))
;; 				 (point))))
;; 	  (t
;; 	   (delete-region (progn (forward-visible-line 0) (point))
;; 			  (progn (forward-visible-line arg) (point))))))

;; (defun js--apply-rcs-patch (patch-buffer)
;;   "Apply an RCS-formatted diff from PATCH-BUFFER to the current buffer."
;;   (let ((target-buffer (current-buffer))
;; 	;; Relative offset between buffer line numbers and line numbers
;; 	;; in patch.
;; 	;;
;; 	;; Line numbers in the patch are based on the source file, so
;; 	;; we have to keep an offset when making changes to the
;; 	;; buffer.
;; 	;;
;; 	;; Appending lines decrements the offset (possibly making it
;; 	;; negative), deleting lines increments it. This order
;; 	;; simplifies the forward-line invocations.
;; 	(line-offset 0))
;;     (save-excursion
;;       (with-current-buffer patch-buffer
;; 	(goto-char (point-min))
;; 	(while (not (eobp))
;; 	  (unless (looking-at "^\\([ad]\\)\\([0-9]+\\) \\([0-9]+\\)")
;; 	    (error "invalid rcs patch or internal error in js--apply-rcs-patch"))
;; 	  (forward-line)
;; 	  (let ((action (match-string 1))
;; 		(from (string-to-number (match-string 2)))
;; 		(len  (string-to-number (match-string 3))))
;; 	    (cond
;; 	     ((equal action "a")
;; 	      (let ((start (point)))
;; 		(forward-line len)
;; 		(let ((text (buffer-substring start (point))))
;; 		  (with-current-buffer target-buffer
;; 		    (decf line-offset len)
;; 		    (goto-char (point-min))
;; 		    (forward-line (- from len line-offset))
;; 		    (insert text)))))
;; 	     ((equal action "d")
;; 	      (with-current-buffer target-buffer
;; 		(js--goto-line (- from line-offset))
;; 		(incf line-offset len)
;; 		(js--delete-whole-line len)))
;; 	     (t
;; 	      (error "invalid rcs patch or internal error in js--apply-rcs-patch")))))))))

;; (custom-set-variables
;;  ;; custom-set-variables was added by Custom.
;;  ;; If you edit it by hand, you could mess it up, so be careful.
;;  ;; Your init file should contain only one such instance.
;;  ;; If there is more than one, they won't work right.
;;  '(ansi-color-faces-vector
;; 	 [default bold shadow italic underline bold bold-italic bold])
;;  '(custom-safe-themes
;; 	 '("8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" "c335adbb7d7cb79bc34de77a16e12d28e6b927115b992bccc109fb752a365c72" "d677ef584c6dfc0697901a44b885cc18e206f05114c8a3b7fde674fce6180879" default))
;;  '(package-selected-packages '(poet-theme)))


;(require 'term)
;(define-key term-mode-map (kbd "C-x C-k") 'term-char-mode)
;(global-set-key (kbd "C-c C-t") (lambda () (interactive) (ansi-term "/bin/bash")))


(cond
 ((eq system-type 'windows-nt)
	(require 'powershell)
	(prefer-coding-system 'utf-8)
	(autoload 'powershell "powershell" "Run powershell as a shell within emacs.")

	(defun git-bash () (interactive)
				 (let ((explicit-shell-file-name "C:/Program Files/Git/bin/bash.exe"))
					 (call-interactively 'shell)))

	(set-frame-parameter (selected-frame) 'alpha '(100 100))
	(add-to-list 'default-frame-alist '(alpha 100 100))

	(set-terminal-coding-system 'utf-8)
	(set-language-environment 'utf-8)
	(set-keyboard-coding-system 'utf-8)
	(prefer-coding-system 'utf-8)
	(setq locale-coding-system 'utf-8)
	(set-default-coding-systems 'utf-8)
	(set-terminal-coding-system 'utf-8)
	(tool-bar-mode -1)
	(scroll-bar-mode -1)
	(setq exec-path (append exec-path '("C:/Program Files (x86)/GnuWin32/bin")))
	(setq visible-bell t)
	(global-set-key (kbd "C-x C-y") 'clipboard-yank)
 )

 (t
	;; transparency
	(set-frame-parameter (selected-frame) 'alpha '(85 . 50))
	(add-to-list 'default-frame-alist '(alpha . (85 . 50)))
	(set-frame-parameter (selected-frame) 'alpha '(85 . 50))
	(add-to-list 'default-frame-alist '(alpha . (85 . 50)))

	(add-to-list 'load-path "~/emacs-libvterm")
	(require 'vterm)
	(global-set-key (kbd "C-c C-t") 'vterm)
	(global-set-key (kbd "C-x C-j") 'vterm-copy-mode)
	(global-set-key (kbd "C-x C-k") 'vterm-copy-mode)

	(add-hook
	 'vterm-mode-hook
	 (lambda ()
		 (local-set-key (kbd "C-c C-t") 'vterm) ; override the override
		 (local-set-key (kbd "C-x C-x") 'vterm-send-C-x)
		 (local-set-key (kbd "C-c C-c") 'vterm-send-C-c)
		 (local-set-key (kbd "C-x C-c") 'vterm-send-C-c)
		 (local-set-key (kbd "C-x C-s") 'vterm-send-C-s)
		 (local-set-key (kbd "C-c") 'vterm-send-C-c)
		 (local-set-key (kbd "C-g") 'vterm-send-C-g)
		 (setq-local show-trailing-whitespace nil)
		 (setq vterm-max-scrollback 50000)))

	)
)



(global-set-key (kbd "C-x ,") 'rename-buffer)

(show-paren-mode 1)
(require 'paren)
(set-face-background 'show-paren-match (face-background 'default))
(set-face-foreground 'show-paren-match "#000")
(set-face-attribute 'show-paren-match nil :weight 'extra-bold)
(setq show-paren-delay 0)

;; (set-face-attribute
;;  'mode-line nil
;;  :foreground "#ffffff"
;;  :background "#111111")

;; (set-face-attribute
;;  'mode-line-inactive nil
;;  :foreground "#111111")


;; (set-face-attribute 'modeline-buffer-id nil :foreground "#00ffff")

(term-set-escape-char 24) ; set term command char to C-x (as it normally is; defaults to C-c)
(setq tramp-default-method "ssh") ; TRAMP defaults to scp, which is slower

;;
;; tabs
;;

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

(add-hook 'web-mode-hook (lambda () (standard-display-ascii ?\t "» ")))
;; (setq default-tab-width 2)

;; (add-hook 'go-mode-hook (lambda () (standard-display-ascii ?\t "\t")))
;; (standard-display-ascii ?\t "\t") ; debug
(setq-default tab-width 2)

(setq grep-save-buffers nil)

;; use hex editor for binary files, instead of jacking up the terminal
;; from https://emacs.stackexchange.com/a/10297/14204
(defun buffer-binary-p (&optional buffer)
  "Return whether BUFFER or the current buffer is binary.

A binary buffer is defined as containing at least on null byte.

Returns either nil, or the position of the first null byte."
  (with-current-buffer (or buffer (current-buffer))
    (save-excursion
      (goto-char (point-min))
      (search-forward (string ?\x00) nil t 1))))

(defun hexl-if-binary ()
  "If `hexl-mode' is not already active, and the current buffer
is binary, activate `hexl-mode'."
  (interactive)
  (unless (eq major-mode 'hexl-mode)
    (when (buffer-binary-p)
      (hexl-mode))))
(add-hook 'find-file-hooks 'hexl-if-binary)


;; transparency

;;(set-frame-parameter (selected-frame) 'alpha '(<active> . <inactive>))
;;(set-frame-parameter (selected-frame) 'alpha <both>)


;; (global-set-key (kbd "C-c t") 'toggle-transparency)

;; (add-to-list 'initial-frame-alist '(alpha 85 95))
;; (add-to-list 'default-frame-alist '(alpha 85 95))
;; (set-frame-parameter nil 'alpha '(85 95))

;; (defun on-after-init ()
;;   (unless (display-graphic-p (selected-frame))
;;     (set-face-background 'default "unspecified-bg" (selected-frame))))

;; (add-hook 'window-setup-hook 'on-after-init)

(desktop-save-mode 1)

;; (load-theme 'poet t)

(defun apply-light-mode ()
	;; (load-theme 'solarized-light t)
	(load-theme 'leuven t)
	(enable-theme 'leuven)
	(custom-set-faces
	 '(my-tab-face ((((class color)) (:foreground "#bbbbb5"))) t))
	(set-face-background 'default "#eeeee5" (selected-frame))
	;; (setq sml/theme 'respectful)
	;; (setq sml/theme 'light)
	;; (setq sml/theme 'light-powerline)
	;; (sml/setup) ;; smart-mode-line
	)

(defun apply-dark-mode ()
	(load-theme 'afternoon t)
	(enable-theme 'afternoon)
	(custom-set-faces
	 '(my-tab-face ((((class color)) (:foreground "#3f3f3fe"))) t))
	;; (set-face-background 'default "#030305" (selected-frame))
	(set-face-background 'default "#050507" (selected-frame))
	;; (setq sml/theme 'powerline)
	;; (setq sml/theme 'dark)
	;; (sml/setup) ;; smart-mode-line
	)

(setq dark-mode 't)

(defun apply-theme ()
	(if (eq dark-mode 't)
		(apply-dark-mode)
		(apply-light-mode)))


;; (set-face-background 'default "unspecified-bg" (selected-frame))

;; (add-hook 'after-init-hook #'apply-theme)

(defun on-after-init ()
	(apply-theme) ;; we need this despite the outer (apply-theme), because the background color gets reset on the first run
	)

(add-hook 'window-setup-hook 'on-after-init)

(apply-theme) ;; we still want this, even with on-after-init, so we can change dark-modoe and reload this file without restarting

(require 'telephone-line)
(telephone-line-mode 1)

(set-display-table-slot standard-display-table 'vertical-border (make-glyph-code ?█))

(display-time-mode 1)
(setq display-time-format "%H:%M")

;; (setq mode-line-front-space ?x)
;; (setq mode-line-end-space ?x)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ispell-dictionary nil)
 '(package-selected-packages
	 '(cmake-mode smart-mode-line-powerline-theme poet-theme leuven-theme go-mode clang-format afternoon-theme)))

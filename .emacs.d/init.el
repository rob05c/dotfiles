(require 'cl)

(add-to-list 'load-path "~/.emacs.d/lisp")
(add-to-list 'load-path "~/.emacs.d/lisp/neotree")

(require 'neotree)
(global-set-key (kbd "C-c n") 'neotree-toggle)

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

(autoload 'elixir-mode "elixir-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.ex\\'" . elixir-mode))
(add-to-list 'auto-mode-alist '("\\.exs\\'" . elixir-mode))

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

(autoload 'dockerfile-mode "dockerfile-mode" "Dockerfile Mode." t)
(add-to-list 'auto-mode-alist '("\\Dockerfile\\'" . dockerfile-mode))

(autoload 'web-mode "web-mode" "Web Mode." t)
(add-to-list 'auto-mode-alist '("\\.html\\'" . web-mode))

(autoload 'arc-mode "arc" "Arc Mode." t)
(add-to-list 'auto-mode-alist '("\\.arc\\'" . arc-mode))

(add-to-list 'auto-mode-alist '("\\.rkt\\'" . scheme-mode))

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

(defun my-go-mode-hook ()
	(require 'go-dlv)
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

(defun my-elixir-hook ()
	(setq indent-tabs-mode nil)
	(setq tab-width 2))
(add-hook 'elixir-mode-hook 'my-elixir-hook)

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
					)))
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


(defun my-js-mode-hook ()
	(add-hook 'before-save-hook 'jsfmt-before-save))
(add-hook 'js-mode-hook 'my-js-mode-hook)

(defun jsfmt-before-save ()
	(jsfmt))

(defcustom jsfmt-command "closure-compiler --formatting=PRETTY_PRINT"
    "The 'jsfmt' closure-compiler formatting command."
    :type 'string
    :group 'js)

(defun jsfmt ()
  "Format the current buffer according to the closure-compiler tool."
  (interactive)
  (let ((tmpfile (make-temp-file "jsfmt" nil ".js"))
	(patchbuf (get-buffer-create "*Jsfmt patch*"))
	(errbuf (if jsfmt-show-errors (get-buffer-create "*Jsfmt Errors*")))
	(coding-system-for-read 'utf-8)
	(coding-system-for-write 'utf-8))

    (save-restriction
      (widen)
      (if errbuf
	  (with-current-buffer errbuf
	    (setq buffer-read-only nil)
	    (erase-buffer)))
      (with-current-buffer patchbuf
	(erase-buffer))

      (write-region nil nil tmpfile)

      ;; We're using errbuf for the mixed stdout and stderr output. This
      ;; is not an issue because closure-compiler --js-output-file does not produce any stdout
      ;; output in case of success.
      (if (zerop (call-process jsfmt-command nil errbuf nil "--js-output-file" tmpfile))
	  (progn
	    (if (zerop (call-process-region (point-min) (point-max) "diff" nil patchbuf nil "-n" "-" tmpfile))
		(message "Buffer is already formatted")
	      (go--apply-rcs-patch patchbuf)
	      (message "Applied jsfmt"))
	    (if errbuf (jsfmt--kill-error-buffer errbuf)))
	(message "Could not apply jsfmt")
	(if errbuf (jsfmt--process-errors (buffer-file-name) tmpfile errbuf)))

      (kill-buffer patchbuf)
      (delete-file tmpfile))))

(defcustom jsfmt-show-errors 'buffer
    "Where to display jsfmt error output.
It can either be displayed in its own buffer, in the echo area, or not at all.

Please note that Emacs outputs to the echo area when writing
files and will overwrite jsfmt's echo output if used from inside
a `before-save-hook'."
    :type '(choice
	    (const :tag "Own buffer" buffer)
	    (const :tag "Echo area" echo)
	    (const :tag "None" nil))
    :group 'js)

(defun jsfmt--process-errors (filename tmpfile errbuf)
  (with-current-buffer errbuf
    (if (eq jsfmt-show-errors 'echo)
	(progn
	  (message "%s" (buffer-string))
	  (jsfmt--kill-error-buffer errbuf))
      ;; Convert the jsfmt stderr to something understood by the compilation mode.
      (goto-char (point-min))
      (insert "jsfmt errors:\n")
      (while (search-forward-regexp (concat "^\\(" (regexp-quote tmpfile) "\\):") nil t)
	(replace-match (file-name-nondirectory filename) t t nil 1))
      (compilation-mode)
      (display-buffer errbuf))))

(defun jsfmt--kill-error-buffer (errbuf)
  (let ((win (get-buffer-window errbuf)))
    (if win
	(quit-window t win)
      (kill-buffer errbuf))))

(defun js--goto-line (line)
  (goto-char (point-min))
  (forward-line (1- line)))

(defun js--delete-whole-line (&optional arg)
    "Delete the current line without putting it in the `kill-ring'.
Derived from function `kill-whole-line'.  ARG is defined as for that
function."
    (setq arg (or arg 1))
    (if (and (> arg 0)
	     (eobp)
	     (save-excursion (forward-visible-line 0) (eobp)))
	(signal 'end-of-buffer nil))
    (if (and (< arg 0)
	     (bobp)
	     (save-excursion (end-of-visible-line) (bobp)))
	(signal 'beginning-of-buffer nil))
    (cond ((zerop arg)
	   (delete-region (progn (forward-visible-line 0) (point))
			  (progn (end-of-visible-line) (point))))
	  ((< arg 0)
	   (delete-region (progn (end-of-visible-line) (point))
			  (progn (forward-visible-line (1+ arg))
				 (unless (bobp)
				   (backward-char))
				 (point))))
	  (t
	   (delete-region (progn (forward-visible-line 0) (point))
			  (progn (forward-visible-line arg) (point))))))

(defun js--apply-rcs-patch (patch-buffer)
  "Apply an RCS-formatted diff from PATCH-BUFFER to the current buffer."
  (let ((target-buffer (current-buffer))
	;; Relative offset between buffer line numbers and line numbers
	;; in patch.
	;;
	;; Line numbers in the patch are based on the source file, so
	;; we have to keep an offset when making changes to the
	;; buffer.
	;;
	;; Appending lines decrements the offset (possibly making it
	;; negative), deleting lines increments it. This order
	;; simplifies the forward-line invocations.
	(line-offset 0))
    (save-excursion
      (with-current-buffer patch-buffer
	(goto-char (point-min))
	(while (not (eobp))
	  (unless (looking-at "^\\([ad]\\)\\([0-9]+\\) \\([0-9]+\\)")
	    (error "invalid rcs patch or internal error in js--apply-rcs-patch"))
	  (forward-line)
	  (let ((action (match-string 1))
		(from (string-to-number (match-string 2)))
		(len  (string-to-number (match-string 3))))
	    (cond
	     ((equal action "a")
	      (let ((start (point)))
		(forward-line len)
		(let ((text (buffer-substring start (point))))
		  (with-current-buffer target-buffer
		    (decf line-offset len)
		    (goto-char (point-min))
		    (forward-line (- from len line-offset))
		    (insert text)))))
	     ((equal action "d")
	      (with-current-buffer target-buffer
		(js--goto-line (- from line-offset))
		(incf line-offset len)
		(js--delete-whole-line len)))
	     (t
	      (error "invalid rcs patch or internal error in js--apply-rcs-patch")))))))))

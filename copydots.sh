#!/bin/bash
# IPv6 addresses must NOT be enclosed in brackets
ssh -4 $* 'mkdir -p ~/.emacs.d/lisp'
scp -4 ~/.bash_profile ~/.profile ~/.emacs ~/.gitconfig ~/.tmux.conf ~/.docker-completion.sh \[$*\]:~
scp -4 ~/.emacs.d/lisp/go-mode.el ~/.emacs.d/lisp/go-mode-autoloads.el ~/.emacs.d/lisp/dockerfile-mode.el ~/.emacs.d/lisp/web-mode.el ~/.emacs.d/lisp/arc.el ~/.emacs.d/lisp/web-mode.el ~/.emacs.d/lisp/markdown-mode.el ~/.emacs.d/lisp/smex.el ~/.emacs.d/lisp/string-inflection.el \[$*\]:~/.emacs.d/lisp
scp -4 -r ~/.emacs.d/lisp/neotree \[$*\]:~/.emacs.d/lisp/neotree
scp -4 ~/.emacs.d/init.el \[$*\]:~/.emacs.d

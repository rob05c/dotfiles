#!/bin/bash
ssh $1 'mkdir -p ~/.emacs.d/lisp'
scp ~/.bash_profile ~/.profile ~/.emacs ~/.gitconfig ~/.tmux.conf ~/.docker-completion.sh ~/.git-completion.bash ~/.gitconfig $1:~
scp ~/.emacs.d/lisp/go-mode.el ~/.emacs.d/lisp/go-mode-autoloads.el ~/.emacs.d/lisp/dockerfile-mode.el ~/.emacs.d/lisp/web-mode.el ~/.emacs.d/lisp/arc.el ~/.emacs.d/lisp/web-mode.el ~/.emacs.d/lisp/markdown-mode.el ~/.emacs.d/lisp/smex.el ~/.emacs.d/lisp/string-inflection.el $1:~/.emacs.d/lisp

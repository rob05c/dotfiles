#!/bin/bash
DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
cp $DIR/.profile ~
cp $DIR/.bash_profile ~
cp $DIR/.gitconfig ~
cp $DIR/.tmux.conf ~
cp $DIR/.docker-completion.sh ~
cp $DIR/.git-completion.bash ~
cp $DIR/.gitconfig ~
cp -r $DIR/.emacs.d ~

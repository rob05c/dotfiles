# ~/.profile: executed by the command interpreter for login shells.
# This file is not read by bash(1), if ~/.bash_profile or ~/.bash_login
# exists.
# see /usr/share/doc/bash/examples/startup-files for examples.
# the files are located in the bash-doc package.

# the default umask is set in /etc/profile; for setting the umask
# for ssh logins, install and configure the libpam-umask package.
#umask 022

# set PATH so it includes user's private bin if it exists
if [ -d "$HOME/bin" ] ; then
    PATH="$HOME/bin:$PATH"
fi

GOROOT=/usr/local/go
PATH=$PATH:$GOROOT/bin
GOPATH=$HOME/lang/go
PATH=$PATH:$GOPATH/bin
PATH=$PATH:$HOME/lang/go/src/github.com/mattn/go-v8/v8/include
export PATH
export GOPATH

PS1="\[\033\]\[[1;33m\]\u@\[\033\]\[[1;36m\]\h \[\033\]\[[1;34m\]\w\[\033\]\[[0m\]$ "

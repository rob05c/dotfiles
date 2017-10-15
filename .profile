# ~/.profile: executed by the command interpreter for login shells.
# This file is not read by bash(1), if ~/.bash_profile or ~/.bash_login
# exists.
# see /usr/share/doc/bash/examples/startup-files for examples.
# the files are located in the bash-doc package.

# the default umask is set in /etc/profile; for setting the umask
# for ssh logins, install and configure the libpam-umask package.
#umask 022

#setxkbmap -layout us -variant altgr-intl -option "nodeadkeys,ctrl:nocaps"
#setxkbmap -option "ctrl:nocaps"

# set PATH so it includes user's private bin if it exists
if [ -d "$HOME/bin" ] ; then
    PATH="$HOME/bin:$PATH"
fi

if [ hash brew 2>/dev/null ] && [-f `brew --prefix`/etc/bash_completion ]; then
		. `brew --prefix`/etc/bash_completion
fi

. ~/.docker-completion.sh

alias sudo='sudo env PATH=$PATH $@'
alias run=open
alias ls='ls -G'
alias grepnoc='grep --color=never'
alias grepc='grep --color=always'
alias less='less -R'
alias fixcaps='setxkbmap -option "ctrl:nocaps"'
#alias loff='xrandr --output eDP1 --off && feh --bg-scale ~/sync/img/ngc1300.jpg'
alias stripansi='sed -r "s/\x1B\[([0-9]{1,2}(;[0-9]{1,2})?)?[m|K]//g"'
alias hubsql='psql -h jezebel -U rob -d hubski'
alias jsonpretty='python -m json.tool'
alias jsp=jsonpretty
alias dockerenv='eval "$(docker-machine env default)"'


PS1="\[\033\]\[[0;37m\]\D{%F %T} \[\033\]\[[1;33m\]\u@\[\033\]\[[1;36m\]\h \[\033\]\[[1;34m\]\w\[\033\]\[[0m\]\n$ "

export LESSOPEN="| /usr/local/bin/src-hilite-lesspipe.sh %s"
export LESS=' -R -F -X '

export PATH="/usr/local/Cellar":$PATH
export PATH="/usr/local/bin":$PATH
export PATH="/Applications/Racket/bin":$PATH
#export PATH=$PATH:"/usr/local/Cellar/gnu-sed/4.2.2/libexec/gnubin/sed"
export GOPATH=$HOME/lang/go/
export GOROOT=/usr/local/go
export PATH=$PATH:$GOPATH/bin
export PATH=$PATH:$GOROOT/bin

export PATH=$PATH:$HOME/nim/bin

export PATH=$PATH:/usr/include/ncursesw

case "$-" in
    *i*)
        bind "set mark-symlinked-directories on"
        # disable XOFF shortcut (which disables input) so C-s does forward-incremental-search.
        stty -ixon
        ;;
esac

export HISTSIZE=""
export HISTFILESIZE=""
export HISTCONTROL=ignoreboth

#export PATH=$PATH:/usr/local/go/bin

man() {
    env LESS_TERMCAP_mb=$'\E[01;31m' \
    LESS_TERMCAP_md=$'\E[01;38;5;74m' \
    LESS_TERMCAP_me=$'\E[0m' \
    LESS_TERMCAP_se=$'\E[0m' \
    LESS_TERMCAP_so=$'\E[38;5;246m' \
    LESS_TERMCAP_ue=$'\E[0m' \
    LESS_TERMCAP_us=$'\E[04;38;5;146m' \
    man "$@"
}

alias c2to1='ssh mtorlu9137e@c2-to-01.cdnlab.comcast.net'
alias c2tm1='ssh mtorlu9137e@c2-tm-01.cdnlab.comcast.net'
alias c2tm2='ssh mtorlu9137e@c2-tm-02.cdnlab.comcast.net'
alias c2tr1='ssh mtorlu9137e@c2-tr-01.cdnlab.comcast.net'
alias c2e1='ssh mtorlu9137e@c2-atsec-01.cdnlab.comcast.net'
alias c2e2='ssh mtorlu9137e@c2-atsec-02.cdnlab.comcast.net'
alias c2m1='ssh mtorlu9137e@c2-atsmid-01.cdnlab.comcast.net'
alias c2m1='ssh mtorlu9137e@c2-atsmid-01.cdnlab.comcast.net'
alias c2ts1='ssh mtorlu9137e@c2-tv-01.cdnlab.comcast.net'
alias c2tv1='ssh mtorlu9137e@c2-ts-01.cdnlab.comcast.net'

#curl -v -s -k -X POST --data '{ "u":"'"superroot"'", "p":"'"supersecreterpassward"'" }' http://192.168.99.100:3000/api/1.2/user/login 2>&1 | grep --color=never mojolicious | cut -c27-132

# source ~/git-prompt.sh
# GIT_PS1_SHOWDIRTYSTATE=1
# GIT_PS1_SHOWSTASHSTATE=1
# GIT_PS1_SHOWUNTRACKEDFILES=1
# GIT_PS1_UNTRACKEDFILES=true
# GIT_PS1_SHOWUPSTREAM="auto"
# GIT_PS1_SHOWCOLORHINTS=true
# PS1='\[\033[1;31m\][\[\033[0;37m\]\u\[\033[1;31m\]@\[\033[0;37m\]\h \[\033[1;31m\]: \[\033[0;36m\]\w$(__git_ps1 " (%s)")\[\033[1;31m\]]\[\033[0;37m\]\\$\[\033[0m\] '
# PROMPT_COMMAND="__git_ps1 '\[\033[1;31m\][\[\033[0;37m\]\u\[\033[1;31m\]@\[\033[0;37m\]\h \[\033[1;31m\]: \[\033[0;36m\]\w\[\e[0m\]' '\[\033[1;31m\]]\[\033[0;37m\]\\$\[\033[0m\] '"

alias gitfork='printf "To Sync a Fork with its Source:\ngit remote add upstream https://github.com/source\ngit fetch upstream\ngit checkout master\ngit rebase upstream/master\n"'
alias tmux='tmux -2'

if [ -f ~/.git-completion.bash ]; then
		. ~/.git-completion.bash
fi

alias docker-attach='docker attach --detach-keys="ctrl-t,q"'
alias docker-exec='docker exec --detach-keys="ctrl-t,q"'
alias kubectl="kubectl --kubeconfig=${PWD}/kubeconfig"
alias em="emacs"
alias pgstart="pg_ctl -D /usr/local/var/postgres -l logfile start"

function perf {
  curl -o /dev/null -s -w "%{time_connect} + %{time_starttransfer} = %{time_total}\n" "$1"
}

alias logfromunix="sed -re 's#^([0-9]*\\.[0-9]*)(.*)#echo `gdate -d @\\1` \\2#ge'"

alias clc="closure-compiler"

function jsfmt {
	closure-compiler --formatting PRETTY_PRINT --formatting PRINT_INPUT_DELIMITER --formatting SINGLE_QUOTES --js $1 > jsfmt.js && mv jsfmt.js $1
}

function jdiff {
	colordiff <(jq -S . $1) <(jq -S . $2)
}

fortune | cowsay

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

# . ~/.docker-completion.sh

# source ~/fuzzy_bash_completion
# fuzzy_replace_filedir_xspec
# fuzzy_setup_for_command cd
# fuzzy_setup_for_command ls

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


#PS1="\[\033\]\[[0;90m\]\D{%F %T} \[\033\]\[[1;33m\]\u@\[\033\]\[[1;36m\]\h \[\033\]\[[1;34m\]\w\[\033\]\[[0m\]\n$ "
#PS1="\[\033\]\[[0;90m\D{%F %T} \[\033\]\[[1;33m\]\u@\[\033\]\[[1;36m\]\h \[\033\]\[[1;34m\]\w\[\033\]\[[0m\]\n$ "
#PS1="\[\033[0m\]\D{%F %T} \[\033\]\[[1;33m\]\u@\[\033\]\[[1;36m\]\h \[\033\]\[[1;34m\]\w\[\033\]\[[0m\]\n$ "
PS1='\[\033[0m\]\D{%F %T} \[\033\]\[[1;33m\]\u@\[\033\]\[[1;36m\]\h \[\033\]\[[1;34m\]\w\[\033\]\[[0m\]\n$ '
# PS1="\D{%F %T} \u@\h \w \n$ "

export LESSOPEN="| /usr/local/bin/src-hilite-lesspipe.sh %s"
export LESS=' -R -F -X '

export PATH=$PATH:/usr/local/opt/go/libexec/bin
export PATH="/usr/local/Cellar":$PATH
export PATH="/usr/local/bin":$PATH
export PATH="/Applications/Racket/bin":$PATH
#export PATH=$PATH:"/usr/local/Cellar/gnu-sed/4.2.2/libexec/gnubin/sed"
export GOPATH=$HOME/lang/go/
export PATH=$PATH:$GOPATH/bin
# GOROOT is the location of go packages, not the binary (which is likely a symlink). This is compiled in, and shouldn't be necessary to set until the packages are installed in a different location than the compiler and packager intended.
# export GOROOT=/usr/bin/go
# export PATH=$PATH:$GOROOT/bin

export PATH=$PATH:$HOME/nim/bin

export PATH=$PATH:/usr/include/ncursesw

case "$-" in
    *i*)
        bind "set mark-symlinked-directories on"
        bind "set revert-all-at-newline on"
        bind "set colored-stats on"
        bind "set colored-completion-prefix on"
        bind "set completion-ignore-case on"
        bind "set completion-map-case on"
        bind "set skip-completed-text on"

        # disable XOFF shortcut (which disables input) so C-s does forward-incremental-search.
        stty -ixon
        ;;
esac

export HISTSIZE=""
export HISTFILESIZE=""
export HISTCONTROL=ignoreboth:erasedups
shopt -s histappend
# export PROMPT_COMMAND="${PROMPT_COMMAND:+$PROMPT_COMMAND$'\n'}history -a; history -c; history -r"
PROMPT_COMMAND="history -a;$PROMPT_COMMAND"

shopt -s checkwinsize

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

alias gitfetch='printf "To add someone else'"'"'s branch:\ngit remote add coworker git://path/to/coworkers/repo.git\ngit fetch coworker\ngit checkout --track coworker/foo OR\ngit checkout -b coworker-foo coworker/foo\n"'

alias gdbh='printf "GDB Emacs:\n  REPL:\n    r run\n    q quit\n    C-c C-r  run\n    C-c C-n  next line\n    C-c C-i  step into\n    C-c <    step out\n  SOURCE:\n    C-x C-a C-b set breakpoint\n    C-x C-a C-d delete breakpoint\n"'

alias emregister='printf "C-x r w    C-x r j\n"'

alias tmux='tmux -2'

if [ -f ~/.git-completion.bash ]; then
		. ~/.git-completion.bash
fi

if [ -f /c/ProgramData/chocolatey/bin ]; then
	export PATH="$PATH:/c/ProgramData/chocolatey/bin"
fi

alias docker-attach='docker attach --detach-keys="ctrl-t,q"'
alias docker-exec='docker exec --detach-keys="ctrl-t,q"'
alias kubectl="kubectl --kubeconfig=${PWD}/kubeconfig"
alias em="emacs"
alias sc="screen"
alias pgstart="pg_ctl -D /usr/local/var/postgres -l logfile start"

function perf {
  curl -o /dev/null -s -w "%{time_connect} + %{time_starttransfer} = %{time_total}\n" "$1"
}

# gdate on OS X, date on everything else
if command -v gdate &> /dev/null; then
	alias logfromunix="sed -re 's#^([0-9]*\\.[0-9]*)(.*)#echo `gdate -d @\\1` \\2#ge'"
else
	alias logfromunix="sed -re 's#^([0-9]*\\.[0-9]*)(.*)#echo `date -d @\\1` \\2#ge'"
fi

alias clc="closure-compiler"


function jsfmt {
	closure-compiler --formatting PRETTY_PRINT --formatting PRINT_INPUT_DELIMITER --formatting SINGLE_QUOTES --js $1 > jsfmt.js && mv jsfmt.js $1
}

function jdiff {
	colordiff <(jq -S . $1) <(jq -S . $2)
}

function jdiffarr {
		colordiff <(jq -cS '(.. | arrays) |= sort' $1) <(jq -cS '(.. | arrays) |= sort' $2)
}

# jq --argfile a a.json --argfile b b.json -n '($a | (.. | arrays) |= sort) as $a | ($b | (.. | arrays) |= sort) as $b | $a == $b'


command -v fortune > /dev/null 2>&1 && command -v cowsay > /dev/null 2>&1 && fortune | cowsay

# screen
########

alias scl="screen -list | gtail -n +2 | ghead -n -1 | gtr -s ' ' '\t' | gcut -f2"
alias sca="screen -r"
alias scn="screen -S"

_screen()
{
	local bufs="$(screen -list | gtail -n +2 | ghead -n -1 | gtr -s ' ' '\t' | gcut -f2 | gcut -f2 -d'.' | gtr '\n' ' ')"
	local cur=${COMP_WORDS[COMP_CWORD]}
	COMPREPLY=( $(compgen -W "$bufs" -- $cur) )
}
complete -F _screen screen
complete -F _screen sc
complete -F _screen sca
complete -F _screen scn

if [ -f "~/.ssh/known_hosts" ]; then
	complete -W "$(echo `cat ~/.ssh/known_hosts | cut -f 1 -d ' ' | sed -e s/,.*//g | uniq | grep -v "\["`;)" ssh
fi

export PATH="$HOME/.cargo/bin:$PATH"

if command -v rustc &> /dev/null; then
	export DYLD_LIBRARY_PATH=$(rustc --print sysroot)/lib:$DYLD_LIBRARY_PATH
fi

alias pbc="pbcopy"

export PAGER=less
alias fix='reset; stty sane; tput rs1; clear; echo -e "\033c"'


ANSIBGREEN='\033[1;32m'
ANSICLEAR='\033[0m' # No Color
read -r -d '' GITMINITRUEMSG << ENDOFMESSAGE
${ANSIBGREEN}## find text in all history:${ANSICLEAR}
git log -S 'text-to-find'
git log -S 'text-to-find' -p

${ANSIBGREEN}## rewrite author, committer name and email${ANSICLEAR}
git filter-branch --commit-filter 'if [ "\$GIT_COMMITTER_NAME" = "John Doe" ];
  then export GIT_AUTHOR_NAME="Jane Deer"; export GIT_AUTHOR_EMAIL=jane@deer.com; export GIT_COMMITTER_NAME="Jane Deer"; export GIT_COMMITTER_EMAIL=jane@deer.com;
  fi; git commit-tree "\$@"'

${ANSIBGREEN}## rewrite commit messages${ANSICLEAR}
git filter-branch -f --msg-filter 'sed "s/^/Prefix /g"' HEAD
git filter-branch -f --msg-filter 'sed "s/foo/bar/g"' HEAD

${ANSIBGREEN}## move all files to a subdirectory${ANSICLEAR}
git filter-branch --tree-filter "rm -rf subdir" HEAD # if an undesirable binary/file exists with the same subdir name
git filter-branch -f --tree-filter "mkdir -p subdir && ls | grep -Ev '^subdir\$' | xargs mv -t subdir" HEAD

${ANSIBGREEN}## rewrite text in file, in all history${ANSICLEAR}
filename='filedir/filename.txt' git filter-branch -f --tree-filter 'test -f \$filename && sed -i "s/foo/bar/g" \$filename  || echo “skipping file“' -- --all

${ANSIBGREEN}## delete entire line containing text in file, in all history${ANSICLEAR}
filename='filedir/filename.txt' git filter-branch -f --tree-filter 'test -f \$filename && sed -i "/foo/ d" \$filename  || echo “skipping file“' -- --all

${ANSIBGREEN}## remove file from all history${ANSICLEAR}
git filter-branch --force --index-filter 'git rm --cached --ignore-unmatch dir/undesirable-file.txt' --prune-empty --tag-name-filter cat -- --all

${ANSIBGREEN}## rewrite text in all files, in all history${ANSICLEAR}
git filter-branch -f --tree-filter 'find . -type f -exec sed -i "s#foo#bar#g" {} \; || true' -- --all
ENDOFMESSAGE

alias gitminitrue='printf "${GITMINITRUEMSG}\n\n"'

if [ -f /usr/local/etc/profile.d/z.sh ]; then
	. /usr/local/etc/profile.d/z.sh
fi

if command -v brew &> /dev/null; then
	if [ -f $(brew --prefix)/etc/bash_completion ]; then
		. $(brew --prefix)/etc/bash_completion
	fi
fi

export PATH="$PATH:/Library/Ballerina/ballerina-1.0.0/bin"

export PATH="$HOME/Android/sdk/tools:$PATH"
export PATH="$HOME/Android/sdk/platform-tools:$PATH"
export PATH="$HOME/Android/sdk/tools/bin:$PATH"
export PATH="/opt/adb/platform-tools:$PATH"

if [[ "$OSTYPE" == darwin* ]]; then
	export CPATH="/Library/Developer/CommandLineTools/SDKs/MacOSX.sdk/System/Library/Perl/5.18/darwin-thread-multi-2level/CORE"
fi

# if [[ "$OSTYPE" == linux* ]]; then
# 	echo "is linux"
# fi

# if [[ "$OSTYPE" == msys* ]]; then
# 	echo "is windows"
# fi

export LIBRARY_PATH=$LIBRARY_PATH:/usr/local/opt/openssl/lib/

# ATS dev
if [ -f /opt/rh/devtoolset-7/enable ]; then
  source /opt/rh/devtoolset-7/enable
fi


# export PATH="/usr/local/opt/llvm/bin:$PATH"
# export LDFLAGS="-L/usr/local/opt/llvm/lib"
# export CPPFLAGS="-I/usr/local/opt/llvm/include"
# export PATH="/usr/local/opt/llvm/bin:$PATH"
export PATH="/usr/local/sbin:$PATH"
export PATH="/usr/local/opt/libpq/bin:$PATH"

# for emacs vterm directory tracking
vterm_printf(){
    if [ -n "$TMUX" ] && ([ "${TERM%%-*}" = "tmux" ] || [ "${TERM%%-*}" = "screen" ] ); then
        # Tell tmux to pass the escape sequences through
        printf "\ePtmux;\e\e]%s\007\e\\" "$1"
    elif [ "${TERM%%-*}" = "screen" ]; then
        # GNU screen (screen, screen-256color, screen-256color-bce)
        printf "\eP\e]%s\007\e\\" "$1"
    else
        printf "\e]%s\e\\" "$1"
    fi
}
vterm_prompt_end(){
    vterm_printf "51;A$(whoami)@$(hostname):$(pwd)"
}
PS1=$PS1'\[$(vterm_prompt_end)\]'

export PATH="/usr/local/share/dotnet:$PATH"

#export PATH="/Library/Developer/Toolchains/swift-5.5-DEVELOPMENT-SNAPSHOT-2021-07-16-a.xctoolchain/usr/bin:$PATH"

export IS_PUTTY=true # for windows VM
if [[ $IS_PUTTY == "true" ]]; then
	# export TERM=xterm-256color
	# tic -x -o ~/.terminfo ~/terminfo-24bit.src
	export TERM=xterm-24bits
	# screen stuff
	#termcapinfo xterm 'Co#256:AB=\E[48;5;%dm:AF=\E[38;5;%dm'
	#defbce "on"
fi

export PATH="$HOME/.cargo/bin:$PATH"
if command -v rustc &> /dev/null; then
	eval "$(perl -I$HOME/perl5/lib/perl5 -Mlocal::lib=$HOME/perl5)"
fi

# TODO remove? these were auto-added by things, I don't trust them
export PATH="/usr/local/opt/openssl@1.1/bin:$PATH"
PATH="/Users/rbutts201/perl5/bin${PATH:+:${PATH}}"; export PATH;
PERL5LIB="/Users/rbutts201/perl5/lib/perl5${PERL5LIB:+:${PERL5LIB}}"; export PERL5LIB;
PERL_LOCAL_LIB_ROOT="/Users/rbutts201/perl5${PERL_LOCAL_LIB_ROOT:+:${PERL_LOCAL_LIB_ROOT}}"; export PERL_LOCAL_LIB_ROOT;
PERL_MB_OPT="--install_base \"/Users/rbutts201/perl5\""; export PERL_MB_OPT;
PERL_MM_OPT="INSTALL_BASE=/Users/rbutts201/perl5"; export PERL_MM_OPT;

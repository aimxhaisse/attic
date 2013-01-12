# s. rannou <mxs@sbrk.org>
#
# Bash's init script

# if non-interactive, do not load stuff
if [[ $- != *i* ]] ; then
	return
fi

alias l='ls -l'
alias ll='ls -la'

export MAIL="/home/mxs/mail"
export GOPATH="/home/mxs/code/excalibot/"
export PATH="$PATH:$HOME/bin:/home/mxs/code/excalibot/bin:/usr/lib64/go/bin/:/sbin/"
export TERM="uxterm"
export EDITOR="emacs -nw"

alias emacs="$EDITOR"

function ru {
    setxkbmap ru
}

function fr {
    setxkbmap -layout us -variant intl
}

function en {
    setxkbmap -layout us
}

# default to french, spawning a new term will default to fr (handy to
# switch back from ru)
fr

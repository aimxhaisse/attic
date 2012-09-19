# s. rannou <mxs@sbrk.org>
#
# Bash's init script

# if non-interactive, do not load stuff
if [[ $- != *i* ]] ; then
	return
fi

alias l='ls -l'
alias ll='ls -la'

export PATH="$PATH:$HOME/bin"

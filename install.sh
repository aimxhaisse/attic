#!/usr/bin/env bash
#
# s. rannou <mxs@sbrk.org>
#
# A script to install a new working env with configs, targets desktop;
# could have been a package but I might need this on different distribs.

# raises an error
function error() {
    echo "error: $1"
    exit 1
}

# prints an info
function info() {
    echo "$1"
}

# prints all deps
function doc() {
    echo "these are the deps required by the script:"
    echo -e "\tgnupg (requuired by pw)"
    echo -e "\tstumpwm (required by .xinitrc and scripts/stumpwm)"
    echo -e "\temacs"
}

# checks that deps are installed or already set
function checkdeps() {
    info "Checking deps..."
    for bindep in gpg fetchmail emacs mutt
    do
	which $bindep > /dev/null
	local ret=$?
	if [ $ret -ne 0 ]
	then
	    error "missing dep found: $bindep"
	else
	    info "$bindep found"
	fi
    done
}
 
# links all scripts in ~/bin
function setscripts() {
    info "Setting scripts"
    if [ ! -d ~/bin ]
    then
	mkdir -p ~/bin || error "can't create ~/bin"
    fi
    # use find to get dotfiles
    for f in $(find scripts -type f)
    do
	local from=$(pwd)/$f
	local to=~/bin/$(basename $f)
	if [ ! -L $to ]
	then
	    ln -s $from $to || error "can't link $from to $to"
	    echo "linked $from to $to"
	fi
    done
}

# links all confs in ~/
function setconfs() {
    info "Settings configs"
    # use find to get dotfiles
    for c in $(find confs -type f)
    do
	local from=$(pwd)/$c
	local to=~/$(basename $c)
	if [ ! -L $to ]
	then
	    ln -s $from $to || error "can't link $from to $to"
	    echo "linked $from to $to"
	fi
    done
}

# misc
function setmisc() {
    info "Setting misc" 
    if [ ! -L ~/misc ]
    then
	local from=$(pwd)/misc
	local to=~/misc
	ln -s $from $to || error "can't link $from to $to"
	echo "linked $from to $to"
    fi
}

# emacs
function setemacs() {
    info "Setting emacs"
    if [ ! -L ~/elisp ]
    then	
	local from=$(pwd)/elisp
	local to=~/elisp
	ln -s $from $to || error "can't link $from to $to"
	echo "linked $from to $to"
    fi
}

# let's go
doc
checkdeps || error "missing dep"
setscripts
setconfs
setmisc
setemacs

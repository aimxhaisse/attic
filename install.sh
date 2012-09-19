#!/usr/bin/env bash
#
# s. rannou <mxs@sbrk.org>
#
# A script to install a new working environment with configurations
# Could have been a package but I might need this on different distribs

# raise an error
function error() {
    echo "error: $1"
    exit 1
}

# print an info
function info() {
    echo "$1"
}

# print all deps
function doc() {
    echo "these are the deps required by the script:"
    echo -e "\tgnupg (requuired by pw)"
    echo -e "\tstumpwm (required by .xinitrc and scripts/stumpwm)"
}

# check that deps are installed or already set
function checkdeps() {
    info "Checking deps..."
    for bindep in gpg fetchmail
    do
	which $bindep > /dev/null
	local ret=$?
	if [ $ret -ne 0 ]
	then
	    error "missing dep found: $bindep"
	else
	    einfo "$bindep found"
	fi
    done
}
 
# link all scripts in ~/bin
function setscripts() {
    info "Setting scripts"
    if [ ! -d ~/bin ]
    then
	mkdir -p ~/bin || error "can't create ~/bin"
    fi
    for f in scripts/*
    do
	local from=$(pwd)/$f
	local to=~/bin/$(basename $f)
	if [ ! -L $to ]
	then
	    ln -s $from $to || error "can't link $from to $to"
	    echo "added $f to ~/bin"
	fi
    done
}

# let's go
doc
countdown 5
checkdeps || error "missing dep"
setscripts

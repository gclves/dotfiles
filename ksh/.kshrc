set -o emacs
alias vi=nvim
alias x=startx
alias python=python3

alias weather='curl http://wttr.in/London'

# Lastpass CLI utilities
lpgrep () {
	search_terms="$@"
	lpass ls | grep -i "$search_terms"
}

lp () {
	name="$@"
	lpass show --password "$name" | xclip
}

PS1="\w % "
fortune


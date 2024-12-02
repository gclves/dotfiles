# .bashrc

# Source global definitions
if [ -f /etc/bashrc ]; then
    . /etc/bashrc
fi

export MAIL=/var/mail/gsg
export EDITOR="nvim"

function sanitize() {
    find $1 -type f -exec chmod 644 {} +
    find $1 -type d -exec chmod 755 {} +
}
function backlight() { echo $1 | sudo tee /sys/class/backlight/intel_backlight/brightness; }

BASHRC_D=$HOME/.bash.d
source $BASHRC_D/bootstrap

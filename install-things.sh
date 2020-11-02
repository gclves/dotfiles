#!/bin/sh

MY_USER=gg

fw_update

pkg_add node vlc go git curl stow qutebrowser \
	tmux ripgrep isync st \
	keepassxc anki \
	cascadia-code go-fonts
# IRC?

npm install -g yarn

# Set up doas
usermod -G staff $MY_USER
echo "permit persist keepenv $MY_USER as root" > /etc/doas.conf

# Power management
rcctl enable apmd
rcctl set apmd flags -A

echo "> Now's a good time to run syspatch as root and restart your system"


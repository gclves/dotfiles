#!/bin/sh

log_line() {
    echo $* > /dev/stderr
}

fedora_post_install() {
	sudo dnf update
	rpm_fusion_install
}

rpm_fusion_install() {
	sudo dnf install \
		https://mirrors.rpmfusion.org/free/fedora/rpmfusion-free-release-$(rpm -E %fedora).noarch.rpm \
		https://mirrors.rpmfusion.org/nonfree/fedora/rpmfusion-nonfree-release-$(rpm -E %fedora).noarch.rpm
}

framework_specific() {
	gsettings set org.gnome.mutter experimental-features "['scale-monitor-framebuffer']"
	gsettings set org.gnome.desktop.peripherals.touchpad tap-to-click true
	sudo grubby --update-kernel=ALL --args="nvme.noacpi=1"
	sudo grubby --update-kernel=ALL --args="i915.enable_psr=0"

	# To enable brightness up/down keys
	sudo grubby --update-kernel=ALL --args="module_blacklist=hid_sensor_hub"
}

fingerprint_setup() {
	sudo dnf install fprintd fprintd-pam

	echo | sudo tee -a /usr/lib/systemd/system/fprintd.service <<EOF
[Install]
WantedBy=multi-user.target
EOF

	sudo systemctl restart fprintd.service
	sudo systemctl enable fprintd.service


	# Erase any old fingerprints
	fprintd-delete $USER

	# Enroll your new fingerprint
	fprintd-enroll

	# Verify your new fingerprint
	fprintd-verify

	# Make sure PAM is authenticated for your fingerprint

	sudo authselect enable-feature with-fingerprint
	sudo authselect apply-changes

	echo "Verify that the fingerprint reader is authorized:"
	sudo authselect current
}

apply_dotfiles() {
	echo "TODO: stow -t $HOME -d <~/src/dotfiles or whatever> files"
	# base_path=`dirname $0`
	# echo stow -t $HOME $base_path/*
}

echo "Running post-installation script" > /dev/stderr
echo "You may be prompted for your password." > /dev/stderr

read -p "Do you wish to run the Fedora post-installation? [yN] " yn
case $yn in
	[Yy]* ) fedora_post_install; break;;
	* ) log_line "Skipping Fedora post-installation";;
esac

read -p "Do you wish to run the Framework-specific configuration? [yN] " yn
case $yn in
	[Yy]* ) framework_specific; break;;
	* ) log_line "Skipping Framework-specific configuration";;
esac

read -p "Enroll fingerprints? [yN] " yn
case $yn in
	[Yy]* ) fingerprint_setup; break;;
	* ) log_line "Skipping fingerprints";;
esac

read -p "Apply dotfiles? [yN] " yn
case $yn in
	[Yy]* ) apply_dotfiles; break;;
	* ) log_line "Skipping dotfiles";;
esac


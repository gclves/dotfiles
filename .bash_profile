# .bash_profile
eval `keychain --quiet --eval id_rsa`

if [[ -f $HOME/.bashrc ]]; then
    . $HOME/.bashrc
fi

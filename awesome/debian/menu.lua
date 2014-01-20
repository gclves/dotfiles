-- automatically generated file. Do not edit (see /usr/share/doc/menu/html)

module("debian.menu")

Debian_menu = {}

Debian_menu["Debian_Ajuda"] = {
	{"Info", "x-terminal-emulator -e ".."info"},
	{"TeXdoctk","/usr/bin/texdoctk"},
	{"Xman","xman"},
	{"yelp","/usr/bin/yelp"},
}
Debian_menu["Debian_Aplicativos_Acessibilidade"] = {
	{"Xmag","xmag"},
}
Debian_menu["Debian_Aplicativos_Ciência_Matemática"] = {
	{"Bc", "x-terminal-emulator -e ".."/usr/bin/bc"},
	{"Dc", "x-terminal-emulator -e ".."/usr/bin/dc"},
	{"LibreOffice Math","/usr/bin/libreoffice --math","/usr/share/icons/hicolor/32x32/apps/libreoffice-math.xpm"},
	{"Xcalc","xcalc"},
}
Debian_menu["Debian_Aplicativos_Ciência"] = {
	{ "Matemática", Debian_menu["Debian_Aplicativos_Ciência_Matemática"] },
}
Debian_menu["Debian_Aplicativos_Editores"] = {
	{"Gedit","/usr/bin/gedit","/usr/share/pixmaps/gedit-icon.xpm"},
	{"GVIM","/usr/bin/vim.gnome -g -f","/usr/share/pixmaps/vim-32.xpm"},
	{"Nano", "x-terminal-emulator -e ".."/bin/nano","/usr/share/nano/nano-menu.xpm"},
	{"Xedit","xedit"},
}
Debian_menu["Debian_Aplicativos_Emuladores_de_Terminal"] = {
	{"Gnome Terminal","/usr/bin/gnome-terminal","/usr/share/pixmaps/gnome-terminal.xpm"},
	{"Rxvt-Unicode","rxvt-unicode","/usr/share/pixmaps/urxvt.xpm"},
	{"XTerm","xterm","/usr/share/pixmaps/xterm-color_32x32.xpm"},
	{"XTerm (Unicode)","uxterm","/usr/share/pixmaps/xterm-color_32x32.xpm"},
}
Debian_menu["Debian_Aplicativos_Escritório"] = {
	{"LibreOffice Calc","/usr/bin/libreoffice --calc","/usr/share/icons/hicolor/32x32/apps/libreoffice-calc.xpm"},
	{"LibreOffice Impress","/usr/bin/libreoffice --impress","/usr/share/icons/hicolor/32x32/apps/libreoffice-impress.xpm"},
	{"LibreOffice Writer","/usr/bin/libreoffice --writer","/usr/share/icons/hicolor/32x32/apps/libreoffice-writer.xpm"},
}
Debian_menu["Debian_Aplicativos_Gerenciamento_de_Arquivos"] = {
	{"Baobab","/usr/bin/baobab","/usr/share/pixmaps/baobab.xpm"},
	{"Brasero","/usr/bin/brasero"},
	{"File-Roller","/usr/bin/file-roller","/usr/share/pixmaps/file-roller.xpm"},
	{"Nautilus","/usr/bin/nautilus","/usr/share/pixmaps/nautilus.xpm"},
}
Debian_menu["Debian_Aplicativos_Gerenciamento_de_Dados"] = {
	{"zim","/usr/bin/zim","/usr/share/pixmaps/zim.xpm"},
}
Debian_menu["Debian_Aplicativos_Gráficos"] = {
	{"ImageMagick","/usr/bin/display.im6 logo:","/usr/share/pixmaps/display.im6.xpm"},
	{"LibreOffice Draw","/usr/bin/libreoffice --draw","/usr/share/icons/hicolor/32x32/apps/libreoffice-draw.xpm"},
	{"X Window Snapshot","xwd | xwud"},
}
Debian_menu["Debian_Aplicativos_Programação"] = {
	{"GDB", "x-terminal-emulator -e ".."/usr/bin/gdb"},
	{"Python (v2.7)", "x-terminal-emulator -e ".."/usr/bin/python2.7","/usr/share/pixmaps/python2.7.xpm"},
	{"Python (v3.3)", "x-terminal-emulator -e ".."/usr/bin/python3.3","/usr/share/pixmaps/python3.3.xpm"},
	{"Ruby (irb1.9.1)", "x-terminal-emulator -e ".."/usr/bin/irb1.9.1"},
	{"Tclsh8.5", "x-terminal-emulator -e ".."/usr/bin/tclsh8.5"},
	{"Tclsh8.6", "x-terminal-emulator -e ".."/usr/bin/tclsh8.6"},
	{"TkWish8.6","x-terminal-emulator -e /usr/bin/wish8.6"},
}
Debian_menu["Debian_Aplicativos_Rede_Comunicação"] = {
	{"Irssi", "x-terminal-emulator -e ".."/usr/bin/irssi"},
	{"Remmina","/usr/bin/remmina"},
	{"Telnet", "x-terminal-emulator -e ".."/usr/bin/telnet"},
	{"weechat-curses", "x-terminal-emulator -e ".."/usr/bin/weechat-curses","/usr/share/pixmaps/weechat.xpm"},
	{"Xbiff","xbiff"},
}
Debian_menu["Debian_Aplicativos_Rede_Transferência_de_Arquivos"] = {
	{"Deluge BitTorrent Client","/usr/bin/deluge","/usr/share/pixmaps/deluge.xpm"},
	{"Transmission BitTorrent Client (GTK)","/usr/bin/transmission-gtk","/usr/share/pixmaps/transmission.xpm"},
}
Debian_menu["Debian_Aplicativos_Rede"] = {
	{ "Comunicação", Debian_menu["Debian_Aplicativos_Rede_Comunicação"] },
	{ "Transferência de Arquivos", Debian_menu["Debian_Aplicativos_Rede_Transferência_de_Arquivos"] },
}
Debian_menu["Debian_Aplicativos_Shells"] = {
	{"Bash", "x-terminal-emulator -e ".."/bin/bash --login"},
	{"Dash", "x-terminal-emulator -e ".."/bin/dash -i"},
	{"Sh", "x-terminal-emulator -e ".."/bin/sh --login"},
	{"Zsh", "x-terminal-emulator -e ".."/bin/zsh5"},
}
Debian_menu["Debian_Aplicativos_Sistema_Administração"] = {
	{"DSL/PPPoE configuration tool", "x-terminal-emulator -e ".."/usr/sbin/pppoeconf","/usr/share/pixmaps/pppoeconf.xpm"},
	{"Editres","editres"},
	{"Gnome Control Center","/usr/bin/gnome-control-center",},
	{"pppconfig", "x-terminal-emulator -e ".."su-to-root -p root -c /usr/sbin/pppconfig"},
	{"TeXconfig", "x-terminal-emulator -e ".."/usr/bin/texconfig"},
	{"Xclipboard","xclipboard"},
	{"Xfontsel","xfontsel"},
	{"Xkill","xkill"},
	{"Xrefresh","xrefresh"},
}
Debian_menu["Debian_Aplicativos_Sistema_Ambiente_de_Linguagem"] = {
	{"Input Method Configuration", "x-terminal-emulator -e ".."/usr/bin/im-config"},
}
Debian_menu["Debian_Aplicativos_Sistema_Hardware"] = {
	{"Xvidtune","xvidtune"},
}
Debian_menu["Debian_Aplicativos_Sistema_Monitoramento"] = {
	{"GNOME system monitor","/usr/bin/gnome-system-monitor"},
	{"Pstree", "x-terminal-emulator -e ".."/usr/bin/pstree.x11","/usr/share/pixmaps/pstree16.xpm"},
	{"Top", "x-terminal-emulator -e ".."/usr/bin/top"},
	{"Xconsole","xconsole -file /dev/xconsole"},
	{"Xev","x-terminal-emulator -e xev"},
	{"Xload","xload"},
}
Debian_menu["Debian_Aplicativos_Sistema_Segurança"] = {
	{"Seahorse","/usr/bin/seahorse","/usr/share/pixmaps/seahorse.xpm"},
}
Debian_menu["Debian_Aplicativos_Sistema"] = {
	{ "Administração", Debian_menu["Debian_Aplicativos_Sistema_Administração"] },
	{ "Ambiente de Linguagem", Debian_menu["Debian_Aplicativos_Sistema_Ambiente_de_Linguagem"] },
	{ "Hardware", Debian_menu["Debian_Aplicativos_Sistema_Hardware"] },
	{ "Monitoramento", Debian_menu["Debian_Aplicativos_Sistema_Monitoramento"] },
	{ "Segurança", Debian_menu["Debian_Aplicativos_Sistema_Segurança"] },
}
Debian_menu["Debian_Aplicativos_Som"] = {
	{"Rhythmbox","/usr/bin/rhythmbox","/usr/share/pixmaps/rhythmbox-small.xpm"},
}
Debian_menu["Debian_Aplicativos_Texto"] = {
	{"Character map","/usr/bin/gucharmap"},
}
Debian_menu["Debian_Aplicativos_Vídeo"] = {
	{"Totem","/usr/bin/totem","/usr/share/pixmaps/totem.xpm"},
	{"VLC media player","/usr/bin/qvlc","/usr/share/icons/hicolor/32x32/apps/vlc.xpm"},
}
Debian_menu["Debian_Aplicativos_Visualizadores"] = {
	{"Evince","/usr/bin/evince","/usr/share/pixmaps/evince.xpm"},
	{"Eye of GNOME","/usr/bin/eog","/usr/share/pixmaps/gnome-eog.xpm"},
	{"Shotwell","/usr/bin/shotwell"},
	{"Xditview","xditview"},
	{"XDvi","/usr/bin/xdvi"},
}
Debian_menu["Debian_Aplicativos"] = {
	{ "Acessibilidade", Debian_menu["Debian_Aplicativos_Acessibilidade"] },
	{ "Ciência", Debian_menu["Debian_Aplicativos_Ciência"] },
	{ "Editores", Debian_menu["Debian_Aplicativos_Editores"] },
	{ "Emuladores de Terminal", Debian_menu["Debian_Aplicativos_Emuladores_de_Terminal"] },
	{ "Escritório", Debian_menu["Debian_Aplicativos_Escritório"] },
	{ "Gerenciamento de Arquivos", Debian_menu["Debian_Aplicativos_Gerenciamento_de_Arquivos"] },
	{ "Gerenciamento de Dados", Debian_menu["Debian_Aplicativos_Gerenciamento_de_Dados"] },
	{ "Gráficos", Debian_menu["Debian_Aplicativos_Gráficos"] },
	{ "Programação", Debian_menu["Debian_Aplicativos_Programação"] },
	{ "Rede", Debian_menu["Debian_Aplicativos_Rede"] },
	{ "Shells", Debian_menu["Debian_Aplicativos_Shells"] },
	{ "Sistema", Debian_menu["Debian_Aplicativos_Sistema"] },
	{ "Som", Debian_menu["Debian_Aplicativos_Som"] },
	{ "Texto", Debian_menu["Debian_Aplicativos_Texto"] },
	{ "Vídeo", Debian_menu["Debian_Aplicativos_Vídeo"] },
	{ "Visualizadores", Debian_menu["Debian_Aplicativos_Visualizadores"] },
}
Debian_menu["Debian_Development"] = {
	{"Brackets","/usr/bin/brackets","/opt/brackets/appshell32.png"},
}
Debian_menu["Debian_Jogos_Brinquedos"] = {
	{"Oclock","oclock"},
	{"Xclock (analog)","xclock -analog"},
	{"Xclock (digital)","xclock -digital -update 1"},
	{"Xeyes","xeyes"},
	{"Xlogo","xlogo"},
}
Debian_menu["Debian_Jogos_Cartas"] = {
	{"Gnome Solitaire Games","/usr/games/sol","/usr/share/pixmaps/aisleriot.xpm"},
}
Debian_menu["Debian_Jogos"] = {
	{ "Brinquedos", Debian_menu["Debian_Jogos_Brinquedos"] },
	{ "Cartas", Debian_menu["Debian_Jogos_Cartas"] },
}
Debian_menu["Debian"] = {
	{ "Ajuda", Debian_menu["Debian_Ajuda"] },
	{ "Aplicativos", Debian_menu["Debian_Aplicativos"] },
	{ "Development", Debian_menu["Debian_Development"] },
	{ "Jogos", Debian_menu["Debian_Jogos"] },
}

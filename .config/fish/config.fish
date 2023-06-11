set fish_greeting # Suppresses fish's intro message
fish_add_path -aP $HOME/.local/bin $HOME/.local/bin/myscripts
fish_add_path -aP $HOME/.local/share/npm/bin $HOME/.local/share/golib/bin $HOME/.local/share/luarocks/bin
fish_add_path -aP $HOME/.config/emacs/bin $HOME/Programming/Flutter/flutter/bin

set -x EDITOR nvim

# pyenv init --path | source
# pyenv init - | source
# zoxide init fish --cmd j | source

set -g fish_cursor_default block
set -g fish_cursor_insert line
set -g fish_cursor_visual underscore
set fish_prompt_pwd_full_dirs 2

set -U SXHKD_SHELL /usr/bin/sh
starship init fish | source
fnm env --use-on-cd | source
# Keybindings
# Refer: https://fishshell.com/docs/3.2/cmds/bind.html
# fish_key_reader to view keycode and bind -f to view functions
# C-Del to kill word
bind \e\[3\;5~ kill-bigword
# C-w to delete word including -,+
bind \cw backward-kill-bigword
# C- Left
bind \e\[1\;5D backward-bigword
# C-Right
bind \e\[1\;5C forward-bigword
# M-k
bind -M insert \ek accept-autosuggestion
# C-backspace
bind -M insert \b backward-kill-word

set fish_greeting # Suppresses fish's intro message
fish_add_path -aP $HOME/.local/bin $HOME/.local/bin/myscripts
fish_add_path -aP $HOME/.local/share/npm/bin $HOME/.local/share/golib/bin $HOME/.local/share/luarocks/bin

set -x EDITOR nvim
set -g fish_cursor_default block
set -g fish_cursor_insert line
set -g fish_cursor_visual underscore
set fish_prompt_pwd_full_dirs 2
set -U SXHKD_SHELL /usr/bin/sh

starship init fish | source
zoxide init fish | source
mise activate fish | source
#fnm env --use-on-cd | source
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
# C-j
bind -M insert \n accept-autosuggestion
# C-backspace
bind -M insert \b backward-kill-word
bind -M insert \cl 'history merge; commandline -f clear-screen'
bind -M visual \cl 'history merge; commandline -f clear-screen'
complete -c dnf5 -w dnf

# bun
set --export BUN_INSTALL "$HOME/.local/share/bun"
set --export PATH $BUN_INSTALL/bin $PATH

if status --is-login; and not set -q CARGO_HOME
  bass source ~/.profile
end

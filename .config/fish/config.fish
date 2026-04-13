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

# fzf
if test -x /usr/bin/fzf
    # Prefer the distro build; the local binary reports the same version
    # but lacks actions used by `fzf --fish` Ctrl-R bindings.
    function fzf --wraps /usr/bin/fzf
        command /usr/bin/fzf $argv
    end
end

set -gx FZF_DEFAULT_COMMAND "rg --files --hidden --follow -g '!{node_modules/,.venv/,venv,.git/,.github,dist,android/,ios/,build/,vendor/}'"
set -gx FZF_CTRL_T_COMMAND "$FZF_DEFAULT_COMMAND"
set -gx FZF_ALT_C_COMMAND "fd -t d -H -L --ignore-file ~/.config/git/ignore"
set -gx FZF_DEFAULT_OPTS "--reverse --cycle --marker='+' --keep-right --color=fg:#cdcdcd --color=bg:#141415 --color=hl:#f3be7c --color=fg+:#aeaed1 --color=bg+:#252530 --color=hl+:#f3be7c --color=border:#606079 --color=header:#6e94b2 --color=gutter:#141415 --color=spinner:#7fa563 --color=info:#f3be7c --color=pointer:#aeaed1 --color=marker:#d8647e --color=prompt:#bb9dbd"
set -gx FZF_CTRL_T_OPTS "--preview 'bat --color=always --style=numbers --line-range=:500 {}' --preview-window=right,60%,wrap"
set -gx FZF_ALT_C_OPTS "--preview 'eza --tree --color=always --icons --level=2 {}'"
/usr/bin/fzf --fish | source
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

# Added by codebase-memory-mcp install
export PATH="/home/shubham/.local/bin:$PATH"

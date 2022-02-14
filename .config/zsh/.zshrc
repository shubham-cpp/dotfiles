setopt correctall
setopt extendedglob
setopt autolist
setopt rcexpandparam
setopt nocheckjobs nobeep nocaseglob
setopt numericglobsort
setopt appendhistory incappendhistory extendedhistory sharehistory
setopt histignorealldups histignoredups histignorespace histexpiredupsfirst
setopt autocd autopushd pushdignoredups
setopt globdots
setopt local_options

zstyle ':completion:*:*:*:*:*' menu select

zstyle ':completion:*' verbose yes
# zstyle ':completion:*:*:kill:*:*' verbose no
zstyle ':completion:*:*:kill:*:jobs' verbose no
zstyle ':completion:*' matcher-list 'm:{a-zA-Z}={A-Za-z}'
# zstyle ':completion:*' matcher-list '' 'm:{a-zA-Z}={A-Za-z}' 'r:|[._-]=* r:|=*' 'l:|=* r:|=*'
zstyle ':completion:*' list-colors "${(s.:.)LS_COLORS}"
zstyle ':completion:*' rehash true
zstyle ':autocomplete:*' add-space \
    executables aliases functions builtins reserved-words commands
# Speed up completions
# zstyle ':completion:*' accept-exact '*(N)'
zstyle ':completion:*' use-cache true
zstyle ':completion:*' cache-path '~/.cache/zsh_cache'
# zstyle ':completion:*' _expand_alias
zstyle ':completion:*' completer _extensions _expand_alias _complete _approximate
zstyle ':completion:*' squeeze-slashes true
## complete as much as you can ..
# zstyle ':completion:*' completer _complete _list _oldlist _expand _ignored _match _correct _approximate _prefix
HISTFILE=$ZDOTDIR/.zhistory
HISTSIZE=10000
SAVEHIST=5000
HISTDUP=erase

WORDCHARS=${WORDCHARS//\/[&.;]}

# Colored Man Pages
# export LESS_TERMCAP_mb=$'\e[1;32m'
# export LESS_TERMCAP_md=$'\e[1;32m'
# export LESS_TERMCAP_me=$'\e[0m'
# export LESS_TERMCAP_se=$'\e[0m'
# export LESS_TERMCAP_so=$'\e[01;33m'
# export LESS_TERMCAP_ue=$'\e[0m'
# export LESS_TERMCAP_us=$'\e[1;4;31m'

# Theming section
autoload -Uz compinit colors zcalc edit-command-line
autoload -Uz bashcompinit && bashcompinit
zmodload -i zsh/complist

compinit -d ~/.cache/zcompdump
colors

plugin(){
    # timer=$(($(date +%s%N)/1000000))
    source "$1"
    # now=$(($(date +%s%N)/1000000))
    # elapsed=$(($now-$timer))
    # printf "%s s: %s\n" $elapsed $1
}
## Plugins section: Enable fish style features
# Apply different settigns for different terminals
plugin $ZDOTDIR/plugins/lazy_eval.zsh
plugin $ZDOTDIR/plugins/zsh-autosuggestions.zsh
plugin $ZDOTDIR/plugins/zsh-completions.plugin.zsh
plugin $ZDOTDIR/plugins/zsh-history-substring-search.zsh
plugin $ZDOTDIR/plugins/fast-syntax-highlighting/fast-syntax-highlighting.plugin.zsh

ZSH_AUTOSUGGEST_BUFFER_MAX_SIZE=20
ZSH_AUTOSUGGEST_HIGHLIGHT_STYLE='fg=8'

_evalcache luajit $HOME/Downloads/Programs/Cool-Ones/z.lua/z.lua --init zsh enhanced once fzf
_evalcache starship init zsh
# _evalcache zoxide init zsh --cmd j
plugin $ZDOTDIR/alias.zsh
plugin $ZDOTDIR/mfunctions.zsh
plugin $ZDOTDIR/bindings.zsh
# _evalcache pyenv init --path
# _evalcache pyenv init -

#!/bin/zsh

### Added by Zinit's installer
if [[ ! -f $HOME/.local/share/zinit/zinit.git/zinit.zsh ]]; then
    print -P "%F{33} %F{220}Installing %F{33}ZDHARMA-CONTINUUM%F{220} Initiative Plugin Manager (%F{33}zdharma-continuum/zinit%F{220})…%f"
    command mkdir -p "$HOME/.local/share/zinit" && command chmod g-rwX "$HOME/.local/share/zinit"
    command git clone https://github.com/zdharma-continuum/zinit "$HOME/.local/share/zinit/zinit.git" && \
        print -P "%F{33} %F{34}Installation successful.%f%b" || \
        print -P "%F{160} The clone has failed.%f%b"
fi

source "$HOME/.local/share/zinit/zinit.git/zinit.zsh"
autoload -Uz _zinit
(( ${+_comps} )) && _comps[zinit]=_zinit

# Load a few important annexes, without Turbo
# (this is currently required for annexes)
zinit light-mode for \
    zdharma-continuum/zinit-annex-as-monitor \
    zdharma-continuum/zinit-annex-bin-gem-node \
    zdharma-continuum/zinit-annex-patch-dl \
    zdharma-continuum/zinit-annex-rust

### End of Zinit's installer chunk
# Plugin history-search-multi-word loaded with investigating.
zinit load zdharma-continuum/history-search-multi-word
ZSH_AUTOSUGGEST_STRATEGY=(history completion)
# Two regular plugins loaded without investigating.
zinit light zsh-users/zsh-autosuggestions
zinit light zdharma-continuum/fast-syntax-highlighting
zinit light zsh-users/zsh-history-substring-search
zinit light zsh-users/zsh-completions
# zinit light skywind3000/z.lua
zinit light lukechilds/zsh-better-npm-completion
zinit light buonomo/yarn-completion
# Load starship theme
# line 1: `starship` binary as command, from github release
# line 2: starship setup at clone(create init.zsh, completion)
# line 3: pull behavior same as clone, source init.zsh
zinit ice as"command" from"gh-r" \
          atclone"./starship init zsh > init.zsh; ./starship completions zsh > _starship" \
          atpull"%atclone" src"init.zsh"
zinit light starship/starship
# zinit ice as"command" from"gh-r" mv"rtx-v* -> rtx" \
#           atclone"./rtx activate zsh > rtx_init.sh" \
#           atpull"%atclone" src"rtx_init.sh"
# zinit light jdx/rtx
zinit ice as"command" from"gh-r" \
          atclone"./zoxide init zsh > zo_init.sh" \
          atpull"%atclone" src"zo_init.sh"
zinit light ajeetdsouza/zoxide

setopt correctall complete_in_word auto_param_keys auto_param_slash
setopt extendedglob
setopt autolist
setopt rcexpandparam
setopt numericglobsort 
setopt nocheckjobs nobeep nocaseglob
setopt extendedhistory sharehistory
# setopt appendhistory incappendhistory extendedhistory sharehistory
setopt hist_verify hist_expire_dups_first hist_ignore_dups hist_ignore_space hist_save_no_dups hist_reduce_blanks hist_find_no_dups
# setopt histignorealldups histignoredups histignorespace histexpiredupsfirst
setopt autocd autopushd pushdignoredups
setopt globdots
setopt local_options

fpath=(~/.local/share/zsh/site-functions $fpath)

zstyle ':completion:*:*:*:*:*' menu select
# This is my old
# zstyle ':completion:*' menu yes
zstyle ':completion:*' verbose yes
# zstyle ':completion:*:*:kill:*:*' verbose no
zstyle ':completion:*:*:kill:*:jobs' verbose no
zstyle ':completion:*' matcher-list 'm:{a-zA-Z}={A-Za-z}'  # case-insensitive completion
# zstyle ':completion:*' matcher-list '' 'm:{a-zA-Z}={A-Za-z}' 'r:|[._-]=* r:|=*' 'l:|=* r:|=*'  # partial words completion
zstyle ':completion:*' list-colors "${(s.:.)LS_COLORS}"
zstyle ':completion:*' rehash true
zstyle ':autocomplete:*' add-space \
    executables aliases functions builtins reserved-words commands
# # Speed up completions
zstyle ':completion:*' accept-exact '*(N)'
zstyle ':completion:*' use-cache true
zstyle ':completion:*' cache-path "${XDG_CACHE_HOME:-$HOME/.cache}/zsh_cache"
zstyle ':completion:*' completer _extensions _expand_alias _complete _approximate _prefix
zstyle ':completion:*' squeeze-slashes true
zstyle ':completion:*' group-name ''
zstyle ':completion::(^approximate*):*:functions'   ignored-patterns '_*'    # Ignore completion functions for commands you don't have:
## complete as much as you can ..
zstyle ':completion:*' completer _complete _list _oldlist _expand _ignored _match _correct _approximate _prefix
HISTFILE=$ZDOTDIR/.zhistory
HISTSIZE=50000
SAVEHIST=10000
HISTDUP=erase

WORDCHARS=${WORDCHARS//\/[&.;]}

autoload -Uz compinit
autoload -Uz colors edit-command-line
autoload -Uz bashcompinit && bashcompinit
zmodload -i zsh/complist

source "$ZDOTDIR"/alias.zsh
source "$ZDOTDIR"/bindings.zsh
source "$ZDOTDIR"/mfunctions.zsh

compinit -d "$HOME/.cache/zcompdump"
colors
[ -x /bin/dnf ] && compdef dnf5=dnf

# bun completions
[ -s "/home/shubham/.local/share/bun/_bun" ] && source "/home/shubham/.local/share/bun/_bun"

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

# Load powerlevel10k theme
# zinit ice depth"1" # git clone depth
# zinit light romkatv/powerlevel10k
# eval "$(starship init zsh)"
zinit ice as"command" from"gh-r" \
          atclone"./starship init zsh > init.zsh; ./starship completions zsh > _starship" \
          atpull"%atclone" src"init.zsh"
zinit light starship/starship
setopt correctall complete_in_word auto_param_keys auto_param_slash
setopt extendedglob
setopt autolist
setopt rcexpandparam
setopt numericglobsort 
setopt nocheckjobs nobeep nocaseglob
setopt appendhistory incappendhistory extendedhistory sharehistory
setopt hist_verify hist_save_no_dups hist_reduce_blanks hist_ignore_space hist_find_no_dups
setopt histignorealldups histignoredups histignorespace histexpiredupsfirst
setopt autocd autopushd pushdignoredups
setopt globdots
setopt local_options

fpath=(~/.local/share/zsh/site-functions $fpath)

# zstyle ':autocomplete:*' default-context ''
# zstyle ':autocomplete:*' min-delay 0.0  # number of seconds (float)
# zstyle ':autocomplete:*' min-input 0  # number of characters (integer)
# zstyle ':autocomplete:tab:*' fzf-completion no
# # When completions don't fit on screen, show up to this many lines:
# zstyle ':autocomplete:*' list-lines 16  # (integer)
# # If any of the following are shown at the same time, list them in the order given:
# zstyle ':completion:*:' group-order \
#   expansions history-words options \
#   aliases functions builtins reserved-words \
#   executables local-directories directories suffix-aliases
# # NOTE: This is NOT the order in which they are generated.

# # Zstyles
# zstyle ':autocomplete:tab:*' insert-unambiguous no
# zstyle ':autocomplete:tab:*' widget-style complete-word
# # Add a space after these completions:
# zstyle ':autocomplete:*' add-space executables aliases functions builtins reserved-words commands

zstyle ':completion:*:*:*:*:*' menu select
# This is my old
# zstyle ':completion:*' menu yes select
zstyle ':completion:*' verbose yes
# zstyle ':completion:*:*:kill:*:*' verbose no
zstyle ':completion:*:*:kill:*:jobs' verbose no
zstyle ':completion:*' matcher-list 'm:{a-zA-Z}={A-Za-z}'
# zstyle ':completion:*' matcher-list '' 'm:{a-zA-Z}={A-Za-z}' 'r:|[._-]=* r:|=*' 'l:|=* r:|=*'
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

# Download Znap, if it's not there yet.
# [[ -f $HOME/.cache/zsh-znap/znap.zsh ]] ||
#     git clone --depth 1 -- \
#         https://github.com/marlonrichert/zsh-snap.git $HOME/.cache/zsh-znap

# source $HOME/.cache/zsh-znap/znap.zsh  # Start Znap

# # znap eval starship 'starship init zsh --print-full-init'
# znap prompt "romkatv/powerlevel10k"

# # `znap source` automatically downloads and starts your plugins.
ZSH_AUTOSUGGEST_BUFFER_MAX_SIZE=20
ZSH_AUTOSUGGEST_HIGHLIGHT_STYLE='fg=8'
ZSH_AUTOSUGGEST_STRATEGY=(history completion)
# znap source zsh-users/zsh-autosuggestions
# znap source zsh-users/zsh-history-substring-search
# znap source zdharma-continuum/fast-syntax-highlighting
# znap source zsh-users/zsh-completions zsh-completions.plugin.zsh
# znap source lukechilds/zsh-better-npm-completion
# znap source buonomo/yarn-completion
# znap source skywind3000/z.lua z.lua.plugin.zsh
# # znap source jeffreytse/zsh-vi-mode

# znap eval zlua 'luajit $HOME/Downloads/GitClones/z.lua/z.lua --init zsh enhanced once fzf'
# bindings

# zle -N edit-command-line
# bindkey -M viins "^X^E" edit-command-line
# bindkey -M viins "^y" yank
# # Alt+u to undo
# bindkey -M viins '^[u' undo

# bindkey -M viins '^H' backward-delete-word
# bindkey -M viins "^w" backward-kill-word
# bindkey -M viins "\e\[3\;5~" kill-word
# bindkey -M viins "^[[1;5D" backward-word
# bindkey -M viins "^[[1;5C" forward-word

# bindkey -M viins '^p' history-beginning-search-backward
# bindkey -M viins '^n' history-beginning-search-forward
# # bind UP and DOWN arrow keys to history substring search
# zmodload zsh/terminfo
# bindkey -M viins -M viins '^[[A' history-substring-search-up
# bindkey -M viins -M viins '^[[B' history-substring-search-down
# # Up down in debian
# bindkey -M viins -M viins '^[OA' history-substring-search-up
# bindkey -M viins -M viins '^[OB' history-substring-search-down
# bindkey -M viins -M viins '^[k' autosuggest-accept
# bindkey -M viins -M viins '^[j' history-substring-search-up

zinit light zsh-users/zsh-autosuggestions
zinit light zsh-users/zsh-completions
zinit light zdharma-continuum/fast-syntax-highlighting
zinit light zsh-users/zsh-history-substring-search
zinit light lukechilds/zsh-better-npm-completion
zinit light buonomo/yarn-completion
zinit light skywind3000/z.lua
zinit light mroth/evalcache

autoload -Uz compinit
autoload -Uz colors edit-command-line
autoload -Uz bashcompinit && bashcompinit
zmodload -i zsh/complist

compinit -d "$HOME/.cache/zcompdump"
colors
source "$ZDOTDIR"/alias.zsh
source "$ZDOTDIR"/bindings.zsh
source "$ZDOTDIR"/mfunctions.zsh

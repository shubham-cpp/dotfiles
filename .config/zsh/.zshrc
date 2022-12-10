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
zstyle ':completion:*' cache-path "${XDG_CACHE_HOME:-$HOME/.cache}/zsh_cache"
zstyle ':completion:*' completer _extensions _expand_alias _complete _approximate _prefix
zstyle ':completion:*' squeeze-slashes true
zstyle ':completion:*' group-name ''
zstyle ':completion::(^approximate*):*:functions'   ignored-patterns '_*'    # Ignore completion functions for commands you don't have:
## complete as much as you can ..
# zstyle ':completion:*' completer _complete _list _oldlist _expand _ignored _match _correct _approximate _prefix
HISTFILE=$ZDOTDIR/.zhistory
HISTSIZE=50000
SAVEHIST=10000
HISTDUP=erase

WORDCHARS=${WORDCHARS//\/[&.;]}

# autoload -Uz compinit
autoload -Uz colors edit-command-line
# autoload -Uz bashcompinit && bashcompinit
# zmodload -i zsh/complist

# compinit -d "$HOME/.cache/zcompdump"
colors

# Download Znap, if it's not there yet.
[[ -f $HOME/.cache/zsh-znap/znap.zsh ]] ||
    git clone --depth 1 -- \
        https://github.com/marlonrichert/zsh-snap.git $HOME/.cache/zsh-znap

source $HOME/.cache/zsh-znap/znap.zsh  # Start Znap

# znap eval starship 'starship init zsh --print-full-init'
znap prompt "romkatv/powerlevel10k"

# `znap source` automatically downloads and starts your plugins.
ZSH_AUTOSUGGEST_BUFFER_MAX_SIZE=20
ZSH_AUTOSUGGEST_HIGHLIGHT_STYLE='fg=8'
ZSH_AUTOSUGGEST_STRATEGY=(history completion)
znap source zsh-users/zsh-autosuggestions
znap source zsh-users/zsh-history-substring-search
znap source zdharma-continuum/fast-syntax-highlighting
znap source zsh-users/zsh-completions zsh-completions.plugin.zsh
znap source lukechilds/zsh-better-npm-completion
znap source buonomo/yarn-completion
znap source skywind3000/z.lua z.lua.plugin.zsh
# znap source jeffreytse/zsh-vi-mode

# `znap function` lets you lazy-load features you don't always need.
znap function _pip_completion pip    'eval "$(pip completion --zsh)"'
# znap function _pipenv         pipenv 'eval "$(pipenv --completion)"'
znap function _nvm            nvm    'source "${NVM_DIR:-$HOME/.config/nvm}"/nvm.sh'

compctl -K    _pip_completion pip
# compdef       _pipenv         pipenv
compdef       _nvm            nvm

# znap eval zlua 'luajit $HOME/Downloads/GitClones/z.lua/z.lua --init zsh enhanced once fzf'
# source ~/Downloads/Gits/z.lua/z.lua.plugin.zsh

source "$ZDOTDIR"/alias.zsh
source "$ZDOTDIR"/bindings.zsh
source "$ZDOTDIR"/mfunctions.zsh

[[ -f ~/.config/tabtab/zsh/__tabtab.zsh ]] && . ~/.config/tabtab/zsh/__tabtab.zsh || true
[[ -f ~/.config/zsh/.p10k.zsh ]] && . ~/.config/zsh/.p10k.zsh || true

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

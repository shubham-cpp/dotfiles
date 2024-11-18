# Set the directory we want to store zinit and plugins
ZINIT_HOME="${XDG_DATA_HOME:-${HOME}/.local/share}/zinit/zinit.git"
# Download Zinit, if it's not there yet
if [ ! -d "$ZINIT_HOME" ]; then
   mkdir -p "$(dirname $ZINIT_HOME)"
   git clone https://github.com/zdharma-continuum/zinit.git "$ZINIT_HOME"
fi

# Source/Load zinit
source "${ZINIT_HOME}/zinit.zsh"

# Add in Powerlevel10k
# zinit ice depth=1; zinit light romkatv/powerlevel10k

fpath+=~/.local/share/zsh/site-functions
autoload -Uz colors edit-command-line
# Smart Url
autoload -Uz url-quote-magic
zle -N self-insert url-quote-magic
autoload -Uz compinit && compinit
zt(){ zinit depth3 lucid ${1/#[0-9][a-c]/wait${1}} ${@:2}; }
# Add in zsh plugins
zinit light zdharma-continuum/fast-syntax-highlighting
zinit light zsh-users/zsh-completions
zinit light zsh-users/zsh-autosuggestions
zinit light zsh-users/zsh-history-substring-search
zinit light QuarticCat/zsh-smartcache
zinit light Aloxaf/fzf-tab

zinit ice as"command" from"gh-r" \
          atclone"./starship init zsh > init.zsh; ./starship completions zsh > _starship" \
          atpull"%atclone" src"init.zsh"
zinit light starship/starship

zinit ice trigger-load!npm wait'0' lucid; zinit light lukechilds/zsh-better-npm-completion
zinit ice trigger-load!man wait'0' lucid; zinit snippet OMZP::colored-man-pages

zinit ice wait"2" as"command" from"gh-r" lucid \
  mv"zoxide*/zoxide -> zoxide" \
  atclone"./zoxide init zsh > zo_init.zsh" \
  atpull"%atclone" src"zo_init.zsh"
zinit light ajeetdsouza/zoxide
# zinit ice wait"2" as"command" from"gh-r" lucid \
#   atclone"./fnm env --use-on-cd > fnmenv.zsh" \
#   atpull"%atclone" pick"fnm" src"fnmenv.zsh" nocompile'!'
# zinit light Schniz/fnm
# Add in snippets
zinit snippet OMZP::git
# zinit snippet OMZP::sudo
# zinit snippet OMZP::archlinux
# Load completions
# autoload -Uz compinit && compinit
zinit cdreplay -q

# History
export ZSH_CACHE_DIR=$HOME/.cache/zsh-cache
HISTSIZE=5000
HISTFILE=~/.cache/zhistory
SAVEHIST=$HISTSIZE
HISTDUP=erase
WORDCHARS=${WORDCHARS//\/[&.;]}
setopt appendhistory
setopt sharehistory
setopt hist_ignore_space
setopt hist_ignore_all_dups
setopt hist_save_no_dups
setopt hist_ignore_dups
setopt hist_find_no_dups
setopt auto_cd
setopt auto_pushd
setopt pushd_ignore_dups
setopt pushdminus
setopt extendedglob numericglobsort

# Completion styling
zstyle ':completion:*' matcher-list 'm:{a-z}={A-Za-z}'
# zstyle ':completion:*' menu no
# zstyle ':completion:*' squeeze-slashes true
# zstyle ':fzf-tab:complete:cd:*' fzf-preview 'ls --color $realpath'
# disable sort when completing `git checkout`
zstyle ':completion:*:git-checkout:*' sort false
zstyle ':completion:*' list-colors ${(s.:.)LS_COLORS}
zstyle ':completion:*' menu no
zstyle ':fzf-tab:complete:cd:*' fzf-preview 'eza -1 --color=always $realpath'
zstyle ':fzf-tab:complete:__zoxide_z:*' fzf-preview 'eza -1 --color=always $realpath'
zstyle ':fzf-tab:*' switch-group '<' '>'
zinit snippet ~/Documents/dotfiles/.config/zsh/alias.zsh
zinit snippet ~/Documents/dotfiles/.config/zsh/bindings.zsh
zinit snippet ~/Documents/dotfiles/.config/zsh/mfunctions.zsh

# Shell integrations
# eval "$(fnm env --use-on-cd)"
[ -x "$(which fzf)" ] && smartcache eval fzf --zsh
[ -x "$(which register-python-argcomplete)" ] && smartcache eval register-python-argcomplete pipx
[ -x "$(which mise)" ] && smartcache eval $HOME/.local/bin/mise activate zsh
[ -x "$(which rustup)" ] && smartcache comp rustup completions zsh

# Ensure unique path
typeset -gU cdpath fpath mailpath path

# bun completions
[ -s "/home/shubham/.local/share/bun/_bun" ] && source "/home/shubham/.local/share/bun/_bun"

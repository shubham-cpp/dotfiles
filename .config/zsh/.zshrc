# Enable Powerlevel10k instant prompt. Should stay close to the top of ~/.config/zsh/.zshrc.
# Initialization code that may require console input (password prompts, [y/n]
# confirmations, etc.) must go above this block; everything else may go below.
# if [[ -r "${XDG_CACHE_HOME:-$HOME/.cache}/p10k-instant-prompt-${(%):-%n}.zsh" ]]; then
#   source "${XDG_CACHE_HOME:-$HOME/.cache}/p10k-instant-prompt-${(%):-%n}.zsh"
# fi

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
  atpull"%atclone" src"zo_init.zsh" nocompile'!'
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
# eval "$(fzf --zsh)"
# eval "$(zoxide init zsh)"
eval "$(fnm env --use-on-cd)"
smartcache eval fzf --zsh
smartcache eval register-python-argcomplete pipx
smartcache comp rustup completions zsh

# bun completions
# [ -s "/home/shubham/.local/share/bun/_bun" ] && source "/home/shubham/.local/share/bun/_bun"

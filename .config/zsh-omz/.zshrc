# Path to your oh-my-zsh installation.
export ZSH="$ZDOTDIR/ohmyzsh"

# Set name of the theme to load --- if set to "random", it will
# load a random theme each time oh-my-zsh is loaded, in which case,
# to know which specific one was loaded, run: echo $RANDOM_THEME
# See https://github.com/ohmyzsh/ohmyzsh/wiki/Themes
# ZSH_THEME="powerlevel10k/powerlevel10k"
ZSH_THEME=""

# Set list of themes to pick from when loading at random
# Setting this variable when ZSH_THEME=random will cause zsh to load
# a theme from this variable instead of looking in $ZSH/themes/
# If set to an empty array, this variable will have no effect.
# ZSH_THEME_RANDOM_CANDIDATES=( "robbyrussell" "agnoster" )

# Uncomment the following line to use case-sensitive completion.
# CASE_SENSITIVE="true"

# Uncomment the following line to use hyphen-insensitive completion.
# Case-sensitive completion must be off. _ and - will be interchangeable.
# HYPHEN_INSENSITIVE="true"

# Uncomment one of the following lines to change the auto-update behavior
zstyle ':omz:update' mode disabled  # disable automatic updates
# zstyle ':omz:update' mode auto      # update automatically without asking
# zstyle ':omz:update' mode reminder  # just remind me to update when it's time

# Uncomment the following line to change how often to auto-update (in days).
# zstyle ':omz:update' frequency 13

# Uncomment the following line if pasting URLs and other text is messed up.
DISABLE_MAGIC_FUNCTIONS="true"

# Uncomment the following line to disable colors in ls.
# DISABLE_LS_COLORS="true"

# Uncomment the following line to disable auto-setting terminal title.
# DISABLE_AUTO_TITLE="true"

# Uncomment the following line to enable command auto-correction.
ENABLE_CORRECTION="true"

# Uncomment the following line to display red dots whilst waiting for completion.
# You can also set it to another string to have that shown instead of the default red dots.
# e.g. COMPLETION_WAITING_DOTS="%F{yellow}waiting...%f"
# Caution: this setting can cause issues with multiline prompts in zsh < 5.7.1 (see #5765)
# COMPLETION_WAITING_DOTS="true"

# Uncomment the following line if you want to disable marking untracked files
# under VCS as dirty. This makes repository status check for large repositories
# much, much faster.
# DISABLE_UNTRACKED_FILES_DIRTY="true"

# Uncomment the following line if you want to change the command execution time
# stamp shown in the history command output.
# You can set one of the optional three formats:
# "mm/dd/yyyy"|"dd.mm.yyyy"|"yyyy-mm-dd"
# or set a custom format using the strftime function format specifications,
# see 'man strftime' for details.
# HIST_STAMPS="mm/dd/yyyy"
export HISTIGNORE="&:[bf]g:c:clear:history:exit:q:pwd:* --help"
# Would you like to use another custom folder than $ZSH/custom?
# ZSH_CUSTOM=/path/to/new-custom-folder

# ZSH_AUTOSUGGEST_STRATEGY=(history completion)
plugins=(git bun vi-mode zoxide zsh-history-substring-search zsh-autosuggestions zsh-smartcache docker zsh-better-npm-completion fast-syntax-highlighting)
fpath+=${ZSH_CUSTOM:-${ZSH:-~/.oh-my-zsh}/custom}/plugins/zsh-completions/src
fpath+=${XDG_DATA_HOME:-${HOME:-~/}/.local/share}/zsh/site-functions

ZSH_AUTOSUGGEST_STRATEGY=(history completion)
ZSH_AUTOSUGGEST_HIGHLIGHT_STYLE="fg=7"
HISTORY_SUBSTRING_SEARCH_ENSURE_UNIQUE=1

HISTFILE=$HOME/.cache/zhistory

zstyle ':completion:*:*:docker:*' option-stacking yes
zstyle ':completion:*:*:docker-*:*' option-stacking yes

source $ZSH/oh-my-zsh.sh
# check if starship is available as executable in path
if [ -x "$(which starship)" ]; then
  smartcache eval starship init zsh
fi
[ -x $HOME/.local/bin/register-python-argcomplete ] && smartcache eval register-python-argcomplete pipx
if [ -x "$(which fzf)" ]; then
  smartcache eval fzf --zsh
fi

smartcache eval $HOME/.local/bin/mise activate zsh
if [ -x "$(which rustup)" ]; then
  smartcache comp rustup completions zsh
fi
WORDCHARS='*?_-.[]~&;!#$%^(){}<>'

source ~/Documents/dotfiles/.config/zsh/alias.zsh
source ~/Documents/dotfiles/.config/zsh/.zprofile
# pkgfile "command not found" handler
source /usr/share/doc/pkgfile/command-not-found.zsh
source $ZDOTDIR/keys.zsh

alias mkd=take
compdef trash-put=rm
[ -x /bin/exa ] && compdef exa=eza
[ -x /bin/dnf5 ] && compdef dnf5=dnf

bindkey -M viins '^[k' autosuggest-accept
bindkey -M viins '^[[A' history-substring-search-up # or '\eOA'
bindkey -M viins '^[[B' history-substring-search-down # or '\eOB'
bindkey -M vicmd 'k' history-substring-search-up
bindkey -M vicmd 'j' history-substring-search-down
# eval "$(fnm env --use-on-cd)"

if [ -x "$(which yazi)" ]; then
  function yy() {
    local tmp="$(mktemp -t "yazi-cwd.XXXXXX")"
    yazi "$@" --cwd-file="$tmp"
    if cwd="$(cat -- "$tmp")" && [ -n "$cwd" ] && [ "$cwd" != "$PWD" ]; then
      cd -- "$cwd"
    fi
    rm -f -- "$tmp"
  }
fi

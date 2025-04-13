export ZGEN_DIR="${XDG_CACHE_HOME:-$HOME/.cache}/zgenom"
export ZSH_CACHE_DIR="${XDG_CACHE_HOME:-$HOME/.cache}/zsh-cache"

# WORDCHARS=${WORDCHARS//\/[&.;]}
HISTFILE=~/.cache/zhistory
ZSH_TMUX_FIXTERM=true

zstyle ':completion:*:git-checkout:*' sort false
autoload -Uz edit-command-line
# Smart Url
autoload -Uz url-quote-magic
zle -N self-insert url-quote-magic

source "$ZGEN_DIR/zgenom.zsh"

# zgenom autoupdate --background

if ! zgenom saved; then
  zgenom compdef

  zgenom load jandamm/zgenom-ext-eval

  zgenom ohmyzsh lib/git.zsh
  zgenom ohmyzsh lib/completion.zsh
  zgenom ohmyzsh lib/grep.zsh
  zgenom ohmyzsh lib/history.zsh
  zgenom ohmyzsh lib/directories.zsh
  zgenom ohmyzsh plugins/colored-man-pages

  if hash git &>/dev/null; then
    zgenom ohmyzsh plugins/git
    zgenom ohmyzsh plugins/gitfast
  fi
  # if hash mise &>/dev/null; then
  #   zgenom ohmyzsh plugins/mise
  # fi
  if hash tmux &>/dev/null; then
    zgenom ohmyzsh plugins/tmux
  fi
  if hash npm &>/dev/null; then
    zgenom ohmyzsh plugins/npm
  fi
  if hash zoxide &>/dev/null; then
    zgenom ohmyzsh plugins/zoxide
  fi
  if hash starship &>/dev/null; then
    zgenom ohmyzsh plugins/starship
  fi
  if hash fzf &>/dev/null; then
    zgenom ohmyzsh plugins/fzf
  fi
  # zgenom ohmyzsh plugins/rust

  zgenom ohmyzsh --completion plugins/docker-compose
  zgenom load zsh-users/zsh-completions
  zgenom load zsh-users/zsh-autosuggestions
  zgenom load zsh-users/zsh-history-substring-search
  zgenom load zdharma-continuum/fast-syntax-highlighting
  zgenom load lukechilds/zsh-better-npm-completion
  zgenom load QuarticCat/zsh-smartcache
  zgenom load Aloxaf/fzf-tab
  zgenom load jeffreytse/zsh-vi-mode

  # Compile your zsh files
  if test -d "$ZDOTDIR"; then
    zgenom compile "$ZDOTDIR"
  else
    zgenom compile "$HOME/.zshrc"
  fi
  if hash rustup &>/dev/null; then
    zgenom eval --name rustup <<($HOME/.cargo/bin/rustup completions zsh)
  fi

  # if hash fnm &>/dev/null; then
  #   zgenom eval --name mise <<(fnm env --use-on-cd)
  # fi
  # if hash fzf &>/dev/null; then
  #   zgenom eval --name mise <<(fzf --zsh)
  # fi
  if hash mise &>/dev/null; then
    zgenom eval --name mise <<(mise activate zsh)
  fi
  # if hash register-python-argcomplete; &>/dev/null then
  #   zgenom eval --name pipx <<(register-python-argcomplete pipx)
  # fi

  zgenom save
fi
setopt appendhistory
setopt sharehistory

source "$HOME/Documents/dotfiles/.config/zsh/alias.zsh"
source "$HOME/Documents/dotfiles/.config/zsh/bindings.zsh"
source "$HOME/Documents/dotfiles/.config/zsh/mfunctions.zsh"

# bun completions
[ -s "~/.bun/_bun" ] && source "~/.bun/_bun"
# zprof

[ -f "/Users/shubham.pawar01/.ghcup/env" ] && . "/Users/shubham.pawar01/.ghcup/env" # ghcup-env

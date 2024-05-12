alias du="/bin/du -h"
alias df="/bin/df -h"
# alias curl="/bin/curl -O -L -C -"
alias grep="/bin/grep -Ei --color=auto"
alias ss="sudo systemctl"

# Changing "ls" to "exa"
alias ls="exa -l --color=auto --icons --group-directories-first"  # long format
alias ll="exa -al --color=auto --icons --group-directories-first --no-user" # my preferred listing
alias la="exa -a --color=auto --icons --group-directories-first"  # all files and dirs
alias lt="exa -aT --color=auto --icons --group-directories-first" # tree listing
alias l="ll --no-filesize --no-permissions --no-time"

alias rr="/bin/rm -rf"
alias rm="trash-put"
alias rmd="trash-put -rf"
alias tls="trash-list"

# # alias d="sudo dnf"
# alias di="sudo dnf5 install"
# alias dr="sudo dnf5 remove"
# alias dU="sudo dnf5 upgrade"
# alias dp="dnf5 provides"
# alias ds="dnf5 search"
# alias a="sudo nala"
# alias ai="sudo nala install"
# alias ar="sudo nala remove"
# alias au="sudo nala upgrade"
# alias as="nala search"
# alias in="ai"
# alias un="ar"
# alias sea="nala search"
alias yay="paru"
alias ys="yay -S --noredownload --needed"
alias yr="yay -Rcns"
# alias xin="sudo xbps-install -S"
# alias xr="sudo xbps-remove -R"
# alias xs="xbps-query -R --regex -s"

alias ..="cd .."
alias ...="cd ../.."
alias ....="cd ../../.."
alias v="nvim"
alias va="NVIM_APPNAME=nvim-astro nvim"
alias se="sudo nvim"
alias sel_font="fc-list | fzf | cut -d':' -f2 | cut -c 2- | xclip -i -r -sel clip"

alias xcp="xclip -i -r -sel clip"
alias rg="rg -i"
alias r="ranger"
alias mci="make -j4 && sudo make install clean"

alias gpg-retrieve="gpg --keyserver pool.sks-keyservers.net --recv-keys"

alias tmux="TERM=xterm-256color /usr/bin/tmux"
# some helpful git aliases
alias g='git'

alias ga='git add'
alias gaa='git add --all'

alias gc='git commit -v'
alias gca='git commit -v -a'
alias gcm='git commit -m'

alias gd="git diff"

alias gp='git push'
alias gpu='git push -u origin main'

alias gst='git status'
alias gsb='git status -sb'

alias gupav='git pull --rebase --autostash -v'

alias gcl='git clone'
alias gch='git checkout'

alias p="pnpm"
alias y='yarn --use-yarnrc "$XDG_CONFIG_HOME/yarn/config"'

alias du="/bin/du -h"
alias df="/bin/df -h"
# alias curl="/bin/curl -O -L -C -"
alias grep="/bin/grep -Ei --color=auto"
alias ss="sudo systemctl"

# Changing "ls" to "exa"
alias ls="eza -l --color=auto --icons --group-directories-first"            # long format
alias ll="eza -al --color=auto --icons --group-directories-first --no-user" # my preferred listing
alias la="eza -a --color=auto --icons --group-directories-first"            # all files and dirs
alias lt="eza -aT --color=auto --icons --group-directories-first"           # tree listing
alias l="ll --no-filesize --no-permissions --no-time"

alias rr="/bin/rm -rf"
alias rm="trash"
alias rmd="trash -rf"
alias tls="trash-list"

alias cls="clear"

# # alias d="sudo dnf"
# alias di="sudo dnf install"
# alias dr="sudo dnf remove"
# alias dU="sudo dnf upgrade"
# alias dp="dnf provides"
# alias ds="dnf search"
# alias a="sudo nala"
# alias ar="sudo nala remove"
# alias ai="sudo nala install"
# alias au="sudo nala upgrade"
# alias as="nala search"
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
alias va="env NVIM_APPNAME=nvim-astro nvim"
alias vl="env NVIM_APPNAME=nvim-lazy nvim"
alias vv="env NVIM_APPNAME=nvim-tt nvim"
alias vc="env NVIM_APPNAME=nvim-chad nvim"
alias se="sudoedit"

alias xcp="xclip -i -r -sel clip"
alias mci="make -j$(expr $(nproc) - 1) && sudo make install clean"

alias gpg-retrieve="gpg --keyserver pool.sks-keyservers.net --recv-keys"

# alias tmux="TERM=xterm-256color /usr/bin/tmux"
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
alias y='yazi'

alias sc="./vendor/bin/sail composer"
alias sa="./vendor/bin/sail artisan"

alias du="/bin/du -h"
alias df="/bin/df -h"
# alias curl="/bin/curl -O -L -C -"
alias grep="/bin/grep -Ei --color=auto"
alias ss="sudo systemctl"

# Changing "ls" to "exa"
alias ls="exa -l --color=auto --group-directories-first --icons"  # long format
alias ll="exa -al --color=auto --group-directories-first --icons --no-user" # my preferred listing
alias la="exa -a --color=auto --group-directories-first --icons"  # all files and dirs
alias lt="exa -aT --color=auto --group-directories-first --icons" # tree listing

alias rr="/bin/rm -rf"
alias rm="trash-put"
alias rmd="trash-put -rf"
alias tls="trash-list"

# alias d="sudo dnf"
# alias di="sudo dnf install"
# alias dr="sudo dnf remove"
# alias du="sudo dnf upgrade"
# alias a="sudo aptitude"
# alias ai="sudo aptitude install"
# alias ar="sudo aptitude remove"
# alias au="sudo aptitude update && sudo aptitude full-upgrade && and sudo aptitude autoclean"
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
alias se="sudoedit"
alias sel_font="fc-list | fzf | cut -d':' -f2 | cut -c 2- | xclip -i -r -sel clip"

alias xcp="xclip -i -r -sel clip"
alias rg="rg -i"
alias r="ranger"
# alias fd="fdfind"
# alias bat="batcat"
alias mci="make -j4 && sudo make install clean"

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
alias yarn='yarn --use-yarnrc "$XDG_CONFIG_HOME/yarn/config"'
# if /bin/grep -iq void /etc/issue*; then
#     alias reboot="loginctl reboot"
#     alias poweroff="loginctl poweroff"
# fi

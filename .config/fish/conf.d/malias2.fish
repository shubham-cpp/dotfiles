alias df="/bin/df -h"
alias du="/bin/du -h"
alias free="/bin/free -h"
# alias sudo='/bin/doas'
# alias sudoedit='/bin/doas nvim'

# alias curl="/bin/curl -O -L -C -"
alias grep="/bin/grep -Ei --color=auto"
alias rg="rg -i"
alias r="ranger"

# Changing "ls" to "exa"
alias ls="eza -l --color=auto --icons=auto --group-directories-first "  # long format
alias ll="eza -al --color=auto --group-directories-first --icons --no-user" # my preferred listing
alias la="eza -a --color=auto --icons --group-directories-first "  # all files and dirs
alias lt="eza -aT --color=auto --icons --group-directories-first " # tree listing
alias l="ll --no-filesize --no-permissions --no-time"
# alias lr="exa -aGR --color=auto"

alias rr="/bin/rm -rf"
alias rm="trash-put"
alias rmd="trash-put -rf"
alias tls="trash-list"
alias xcp="xclip -i -r -sel clip"
# alias zrc="$EDITOR $HOME/.config/zsh/alias.zsh"
# alias szrc="source $HOME/.config/zsh/.zshrc"

# alias d="sudo dnf"
alias di="sudo dnf install"
alias dr="sudo dnf remove"
alias dU="sudo dnf upgrade"
alias dp="dnf provides"
alias ds="dnf search"
# # alias a="sudo nala"
# alias ai="sudo nala install"
# alias ar="sudo nala remove"
# alias au="sudo nala upgrade"
# alias as="nala search"
# alias in="ai"
# alias un="ar"
# alias up="a update;and a upgrade;and a autopurge"
# alias sea="nala search"
# alias yay="paru"
# alias ys="yay -S --noredownload --needed"
# alias yr="yay -Rncs"
# # alias xin="sudo xbps-install -S"
# alias xr="sudo xbps-remove -R"
# alias xs="xbps-query -R --regex -s"

alias ..="cd .."
alias ...="cd ../.."
alias merge="xrdb ~/.config/X11/Xresources"
alias v="nvim"
alias vv="NVIM_APPNAME=nvim-my nvim"
alias vl="NVIM_APPNAME=nvim-lazyvim nvim"
alias se="sudo nvim"
alias mci="make -j5 && sudo make install clean"
alias sel_font="fc-list | fzf | cut -d':' -f2 | cut -c 2- | xclip -i -r -sel clip"

#get fastest mirrors in your neighborhood
# alias mirror="sudo reflector -f 30 -l 30 --number 10 --verbose --save /etc/pacman.d/mirrorlist"
# alias mirrord="sudo reflector --latest 50 --number 20 --sort delay --save /etc/pacman.d/mirrorlist"
# alias mirrors="sudo reflector --latest 50 --number 20 --sort score --save /etc/pacman.d/mirrorlist"
# alias mirrora="sudo reflector --latest 50 --number 20 --sort age --save /etc/pacman.d/mirrorlist"

alias gpg-retrieve="gpg --keyserver pool.sks-keyservers.net --recv-keys"

# alias tmux="TERM=xterm-256color /usr/bin/tmux"
# alias which="alias || /bin/which --tty-only --read-alias"
# some helpful git aliases
alias g="git"

alias ga="git add"
alias gaa="git add --all"

alias gch="git checkout"
alias gc="git commit -v"
alias gca="git commit -v -a"
alias gcm="git commit -m"

alias gd="git diff"

alias gp="git push"

alias gst="git status"
alias gsb="git status -sb"

alias gupav="git pull --rebase --autostash -v"

alias gcl="git clone"
alias gclr="git clone --recurse-submodules"

alias p="pnpm"
alias y='y --use-yarnrc "$XDG_CONFIG_HOME/yarn/config"'
# if /bin/grep -iq void /etc/issue*
#     alias reboot="loginctl reboot"
#     alias poweroff="loginctl poweroff"
# end

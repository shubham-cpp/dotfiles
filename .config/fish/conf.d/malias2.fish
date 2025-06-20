alias df="/bin/df -h"
alias du="/bin/du -h"
alias free="/bin/free -h"

# alias curl="/bin/curl -O -L -C -"
alias grep="/bin/grep -Ei --color=auto"

# Changing "ls" to "exa"
if command -q eza
    or command -q exa
    alias ls="eza -l --color=auto --icons --group-directories-first"
    alias ll="eza -al --color=auto --icons --group-directories-first --no-user"
    alias la="eza -a --color=auto --icons --group-directories-first"
    alias lt="eza -aT --color=auto --icons --group-directories-first"
    alias l="ll --no-filesize --no-permissions --no-time"
else
    alias ls="command ls -lhv --classify --color=auto --group-directories-first"
    alias la="command ls -hv --classify --color=auto --group-directories-first"
    alias ll="ls -a"
    alias lt="ls --recursive"
end

alias rr="/bin/rm -rf"
if command -q trash
    alias rm="trash"
    alias rmd="trash-put -rf"
    alias tls="trash-list"
end
alias cls="clear"
alias xcp="xclip -i -r -sel clip"

if command -q dnf
    # alias d="sudo dnf5"
    alias di="sudo dnf install"
    alias dr="sudo dnf remove"
    alias dU="sudo dnf upgrade"
    alias dp="dnf provides"
    alias ds="dnf search"
end
if command -q nala
    # alias a="sudo nala"
    alias ai="sudo nala install"
    alias ar="sudo nala remove"
    alias au="sudo nala upgrade"
    alias as="nala search"
end
# alias up="a update;and a upgrade;and a autopurge"
# alias sea="nala search"
[ -x /usr/bin/paru ] && alias yay="paru"
if command -q paru
    or command -q yay
    alias ys="yay -S --noredownload --needed"
    alias yr="yay -Rncs"
end
if command -q xbps-install
    alias xin="sudo xbps-install -S"
    alias xr="sudo xbps-remove -R"
    alias xs="xbps-query -R --regex -s"
end

alias ..="cd .."
alias ...="cd ../.."
alias merge="xrdb ~/.config/X11/Xresources"
alias v="nvim"
alias va="env NVIM_APPNAME=nvim-astro nvim"
alias vl="env NVIM_APPNAME=nvim-lazy nvim"
alias vv="env NVIM_APPNAME=nvim-tt nvim"
alias vc="env NVIM_APPNAME=nvim-chad nvim"
alias se="sudoedit"
alias mci="make -j(expr (nproc) - 1) && sudo make install clean"

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

alias p="corepack pnpm"
alias y='yazi'

alias sc="./vendor/bin/sail composer"
alias sa="./vendor/bin/sail artisan"
# if /bin/grep -iq void /etc/issue*
#     alias reboot="loginctl reboot"
#     alias poweroff="loginctl poweroff"
# end

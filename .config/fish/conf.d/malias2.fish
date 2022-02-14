alias df="df -h"
alias du="/bin/du -h"
alias free="/bin/free -h"

alias curl="/bin/curl -O -L -C -"
alias grep="/bin/grep -Ei --color=auto"
alias rg="rg -i"

# Changing "ls" to "exa"
alias ls="exa -l --color=auto --group-directories-first --icons"
alias ll="exa -al --color=auto --group-directories-first --icons --no-user"
alias la="exa -a --color=auto --group-directories-first --icons"
alias lt="exa -aT --color=auto --group-directories-first --icons"
# alias lr="exa -aGR --color=auto"

alias rr="/bin/rm -rf"
alias rm="trash-put"
alias rmd="trash-put -rf"
alias tls="trash-list"
alias xcp="xclip -i -r -sel clip"
# alias zrc="$EDITOR $HOME/.config/zsh/alias.zsh"
# alias szrc="source $HOME/.config/zsh/.zshrc"

alias yay="paru"
alias ys="yay -S --noredownload --needed"
alias yr="yay -Rncs"
# alias xin="sudo xbps-install -S"
# alias xr="sudo xbps-remove -R"
# alias xs="xbps-query -R --regex -s"

alias ..="cd .."
alias ...="cd ../.."
alias merge="xrdb ~/.config/X11/Xresources"
alias v="nvim"
alias se="sudoedit"
alias mci="make -j5 && sudo make install clean"
alias sel_font="fc-list | fzf | cut -d':' -f2 | cut -c 2- | xclip -i -r -sel clip"

#get fastest mirrors in your neighborhood
# alias mirror="sudo reflector -f 30 -l 30 --number 10 --verbose --save /etc/pacman.d/mirrorlist"
# alias mirrord="sudo reflector --latest 50 --number 20 --sort delay --save /etc/pacman.d/mirrorlist"
# alias mirrors="sudo reflector --latest 50 --number 20 --sort score --save /etc/pacman.d/mirrorlist"
# alias mirrora="sudo reflector --latest 50 --number 20 --sort age --save /etc/pacman.d/mirrorlist"

alias gpg-retrieve="gpg --keyserver pool.sks-keyservers.net --recv-keys"

alias tmux="TERM=xterm-256color /usr/bin/tmux"
alias which="alias | /bin/which --tty-only --read-alias"
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

if /bin/grep -iq void /etc/issue*
    alias reboot="loginctl reboot"
    alias poweroff="loginctl poweroff"
end

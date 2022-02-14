[[ $- != *i* ]] && return

_set_my_PS1() {
    PS1='[\u@\h \W]\$ '
    if [ "$(whoami)" = "liveuser" ] ; then
        local iso_version="$(grep ^VERSION= /usr/lib/endeavouros-release 2>/dev/null | cut -d '=' -f 2)"
        if [ -n "$iso_version" ] ; then
            local prefix="eos-"
            local iso_info="$prefix$iso_version"
            PS1="[\u@$iso_info \W]\$ "
        fi
    fi
}
_set_my_PS1
unset -f _set_my_PS1

export EDITOR=nvim
export VISUAL=nvim

alias ls='exa -l --color=auto --icons --group-directories-first'
alias ll='ls -a --no-user'
alias v="nvim"
alias se="sudoedit"

# Expand the history size
export HISTFILESIZE=10000
export HISTSIZE=500
export HISTCONTROL=erasedups:ignoredups:ignorespace

shopt -s checkwinsize
shopt -s histappend
shopt -s autocd dirspell interactive_comments
shopt -s globstar extglob dotglob

PROMPT_COMMAND='history -a'

[[ "$(whoami)" = "root" ]] && return

[[ -z "$FUNCNEST" ]] && export FUNCNEST=100

source ~/.config/zsh/alias.zsh

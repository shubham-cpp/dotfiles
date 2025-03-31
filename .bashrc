#!/usr/bin/env bash

[[ $- != *i* ]] && return

_set_my_PS1() {
	PS1='[\u@\h \W]\$ '
}
_set_my_PS1
unset -f _set_my_PS1

complete -cf doas

export EDITOR=nvim
export VISUAL=nvim

alias ls='exa -l --color=auto --icons --group-directories-first'
alias ll='ls -a --no-user'
alias v="nvim"
alias se="sudoedit"

# Expand the history size
export HISTFILESIZE=10000
export HISTSIZE=1000
export HISTCONTROL=ignoredups:erasedups:ignorespace

shopt -s checkwinsize
shopt -s histappend
shopt -s autocd dirspell interactive_comments
shopt -s globstar extglob dotglob

export PROMPT_COMMAND="history -a; history -c; history -r; $PROMPT_COMMAND"

[ -f /usr/share/bash-completion/bash_completion ] && . /usr/share/bash-completion/bash_completion

[[ "$(whoami)" = "root" ]] && return

[[ -z "$FUNCNEST" ]] && export FUNCNEST=100

source "$HOME"/Documents/dotfiles/.config/zsh/alias.zsh

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
export HISTSIZE=5000
export HISTCONTROL=erasedups:ignoredups:ignorespace

shopt -s checkwinsize
shopt -s histappend
shopt -s autocd dirspell interactive_comments
shopt -s globstar extglob dotglob

PROMPT_COMMAND="history -a;$PROMPT_COMMAND"

[[ "$(whoami)" = "root" ]] && return

[[ -z "$FUNCNEST" ]] && export FUNCNEST=100

[ -r "$HOME/.config/zsh/alias.zsh" ] && . "$HOME"/.config/zsh/alias.zsh

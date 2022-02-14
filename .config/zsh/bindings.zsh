## Keybindings section
# TO view keys use 'showkey -a'
# bindkey -e
bindkey -v
export KEYTIMEOUT=1

bindkey -M viins '^[[7~' beginning-of-line                               # Home key
bindkey -M viins '^[[H' beginning-of-line                                # Home key
if [[ "${terminfo[khome]}" != "" ]]; then
    bindkey -M viins "${terminfo[khome]}" beginning-of-line                # [Home] - Go to beginning of line
fi
bindkey -M viins '^[[8~' end-of-line                                     # End key
bindkey -M viins '^[[F' end-of-line                                     # End key
if [[ "${terminfo[kend]}" != "" ]]; then
    bindkey -M viins "${terminfo[kend]}" end-of-line                       # [End] - Go to end of line
fi
bindkey -M viins '^p' history-beginning-search-backward
bindkey -M viins '^n' history-beginning-search-forward

zle -N edit-command-line
bindkey -M viins "^X^E" edit-command-line

bindkey -M viins "^y" yank
# Alt+u to undo
bindkey -M viins '^[u' undo

bindkey -M viins "^u" kill-region
bindkey -M viins "^k" kill-line

bindkey -M viins "^a" beginning-of-line
bindkey -M viins "^e" end-of-line

bindkey -M viins "^[[3~" delete-char
# del delete char
bindkey -M viins "^?" backward-delete-char
# C-Backspace delete word
bindkey -M viins '^H' backward-delete-word
bindkey -M viins "^w" backward-kill-word
bindkey -M viins "\e\[3\;5~" kill-word
bindkey -M viins "^[[1;5D" backward-word
bindkey -M viins "^[[1;5C" forward-word

# bind UP and DOWN arrow keys to history substring search
zmodload zsh/terminfo
bindkey -M viins -M viins '^[[A' history-substring-search-up
bindkey -M viins -M viins '^[[B' history-substring-search-down
bindkey -M viins -M viins '^[k' autosuggest-accept
bindkey -M viins -M viins '^[j' history-substring-search-up

bindkey -M menuselect 'h' vi-backward-char
bindkey -M menuselect 'k' vi-up-line-or-history
bindkey -M menuselect 'j' vi-down-line-or-history
bindkey -M menuselect 'l' vi-forward-char

# bindkey -M vicmd "b" backward-word
# bindkey -M vicmd "w" forward-word

# Change cursor shape for different vi modes.
function zle-keymap-select {
    if [[ ${KEYMAP} == vicmd ]] ||
        [[ $1 = 'block' ]]; then
            echo -ne '\e[1 q'

        elif [[ ${KEYMAP} == main ]] ||
            [[ ${KEYMAP} == viins ]] ||
            [[ ${KEYMAP} = '' ]] ||
            [[ $1 = 'beam' ]]; then
                    echo -ne '\e[5 q'
    fi
}
zle -N zle-keymap-select

# Use beam shape cursor on startup.
echo -ne '\e[5 q'

_fix_cursor() {
   echo -ne '\e[5 q'
}

precmd_functions+=(_fix_cursor)

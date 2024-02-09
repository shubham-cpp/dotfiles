# Alt+u to undo
bindkey -M viins '^[u' undo
bindkey -M viins "^y" yank

bindkey -M viins "^u" kill-region
bindkey -M viins "^k" kill-line

bindkey -M viins '^H' backward-delete-word
bindkey -M viins '^p' history-substring-search-up
bindkey -M viins '^n' history-substring-search-down
# zsh-history-substring-search configuration
bindkey '^[[A' history-substring-search-up # or '\eOA'
bindkey '^[[B' history-substring-search-down # or '\eOB'
# bindkey "$terminfo[kcuu1]" history-substring-search-up
# bindkey "$terminfo[kcud1]" history-substring-search-down
HISTORY_SUBSTRING_SEARCH_ENSURE_UNIQUE=1

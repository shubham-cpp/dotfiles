# Completions for gitc (git clone + cd helper)
complete -c gitc -w "git clone" -d "Clone a repo and cd into it quickly"

# Optional: Add a short description when you type gitc<TAB>
complete -c gitc -n __fish_use_subcommand -a help -d "Show usage"

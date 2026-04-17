chx(){
    chmod 744 "$@"
}

mkd(){
	mkdir -p $@ && cd ${@:$#}|| echo "Please provide a valid directory name"
}

function yy() {
	local tmp="$(mktemp -t "yazi-cwd.XXXXXX")"
	yazi "$@" --cwd-file="$tmp"
	if cwd="$(cat -- "$tmp")" && [ -n "$cwd" ] && [ "$cwd" != "$PWD" ]; then
		cd -- "$cwd"
	fi
	rm -f -- "$tmp"
}
# Adding zsh tab completions for unstaged files
# __git_status_files () {
#   local -a status_files=( ${"${(0)"$(git status -z)"}"} )
#   local -a unstaged_files
#   local -a staged_files
#   for entry in ${status_files}; do
#     local stts=$entry[1,3]
#     local file=$entry[4,-1]
#
#     if [[ $stts[2] != ' ' ]]
#     then
#       unstaged_files+=$file
#     fi
#
#     if [[ $stts[1] != ' ' ]] && [[ $stts[1] != '?' ]]
#     then
#       staged_files+=$file
#     fi
#   done
#
#   _describe -t unstaged 'Unstaged' unstaged_files && ret=0
#   _describe -t staged 'Staged' staged_files && ret=0
#
#   return $ret
# }
#
# __git_staged_files () {
#   local -a staged_files=( ${"${(0)"$(git diff-index -z --name-only --no-color --cached HEAD)"}"} )
#   _describe -t staged 'Staged files' staged_files && ret=0
#   return $ret
# }
#
# __git_modified_files () {
#   __git_status_files
# }
#
# __git_treeish-to-index_files () {
#   __git_staged_files
# }
#
# __git_other_files () {
# }

gitc() {
  if [ $# -eq 0 ]; then
    echo "Usage: gitc [git-clone-options...] <repository> [directory]"
    echo "  Quickly clone a repo and cd into it"
    echo
    echo "Examples:"
    echo "  gitc https://github.com/user/repo.git"
    echo "  gitc --depth=1 https://github.com/vercel/next.js.git"
    echo "  gitc -b dev --single-branch git@github.com:user/project.git my-project"
    return 1
  fi

  local last_arg="${@: -1}"
  local second_last_arg="${@: -2:1}"

  if [[ "$last_arg" =~ ^(git@|https?://|ssh://|[a-zA-Z0-9][a-zA-Z0-9.-]+/[a-zA-Z0-9._/-]+) ]]; then
    local repo_url="$last_arg"
    local target_dir=""
    local clone_args=("${@:1:$#-1}")
  else
    local repo_url="$second_last_arg"
    local target_dir="$last_arg"
    local clone_args=("${@:1:$#-2}")
  fi

  if [ -z "$repo_url" ]; then
    echo "Error: No repository URL found"
    echo "Last arguments were: '$second_last_arg' '$last_arg'"
    return 1
  fi

  echo "→ Running: git clone ${clone_args[*]} $repo_url ${target_dir:+\"$target_dir\"}"

  git clone "${clone_args[@]}" "$repo_url" ${target_dir:+"$target_dir"}

  if [ $? -ne 0 ]; then
    echo "Clone failed :("
    return 1
  fi

  local cd_dir
  if [ -n "$target_dir" ]; then
    cd_dir="$target_dir"
  else
    cd_dir="${repo_url##*/}"
    cd_dir="${cd_dir%.git}"
  fi

  if [ ! -d "$cd_dir" ]; then
    echo "Warning: Directory '$cd_dir' not found after clone"
    return 1
  fi

  echo "→ Entering: $cd_dir"
  cd "$cd_dir" || { echo "Failed to cd into $cd_dir"; return 1; }
  git branch --show-current 2>/dev/null || true
}

gdd() {
  preview="git diff $@ --color=always -- {-1}"
  git diff $@ --name-only | fzf -m --ansi --preview $preview | xargs -r git add
}
autoload -U select-quoted
zle -N select-quoted
for m in visual viopp; do
  for c in {a,i}{\',\",\`}; do
    bindkey -M $m $c select-quoted
  done
done

autoload -U select-bracketed
zle -N select-bracketed
for m in visual viopp; do
  for c in {a,i}{\{,\[,\(}; do
    bindkey -M $m $c select-bracketed
  done
done

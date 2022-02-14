chx(){
    chmod +x "$@"
}

mkd(){
	mkdir -p $@ && cd ${@:$#}|| echo "Please provide a valid directory name"
}
# Adding zsh tab completions for unstaged files
__git_status_files () {
  local -a status_files=( ${"${(0)"$(git status -z)"}"} )
  local -a unstaged_files
  local -a staged_files
  for entry in ${status_files}; do
    local stts=$entry[1,3]
    local file=$entry[4,-1]

    if [[ $stts[2] != ' ' ]]
    then
      unstaged_files+=$file
    fi

    if [[ $stts[1] != ' ' ]] && [[ $stts[1] != '?' ]]
    then
      staged_files+=$file
    fi
  done

  _describe -t unstaged 'Unstaged' unstaged_files && ret=0
  _describe -t staged 'Staged' staged_files && ret=0

  return $ret
}

__git_staged_files () {
  local -a staged_files=( ${"${(0)"$(git diff-index -z --name-only --no-color --cached HEAD)"}"} )
  _describe -t staged 'Staged files' staged_files && ret=0
  return $ret
}

__git_modified_files () {
  __git_status_files
}

__git_treeish-to-index_files () {
  __git_staged_files
}

__git_other_files () {
}

gdd() {
  preview="git diff $@ --color=always -- {-1}"
  git diff $@ --name-only | fzf -m --ansi --preview $preview | xargs -r git add
}

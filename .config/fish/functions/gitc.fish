function gitc --description "Clone a repo and cd into it"
    if test (count $argv) -eq 0
        echo "Usage: gitc [git-clone-options...] <repository> [directory]"
        echo "  Quickly clone a repo and cd into it"
        echo
        echo "Examples:"
        echo "  gitc https://github.com/user/repo.git"
        echo "  gitc --depth=1 https://github.com/vercel/next.js.git"
        echo "  gitc -b dev --single-branch git@github.com:user/project.git my-project"
        return 1
    end

    set -l last_arg $argv[-1]
    set -l second_last_arg $argv[-2]

    # If last argument looks like a URL or repo path, no custom directory
    set -l repo_url ""
    set -l target_dir ""
    set -l clone_args

    if string match -qr '^(git@|https?://|ssh://|[a-zA-Z0-9][a-zA-Z0-9.-]+/[a-zA-Z0-9._/-]+)' $last_arg
        set repo_url $last_arg
        set clone_args $argv[1..-2]
    else
        # Last arg is probably target directory
        set repo_url $second_last_arg
        set target_dir $last_arg
        set clone_args $argv[1..-3]
    end

    if test -z "$repo_url"
        echo "Error: No repository URL found"
        echo "Last arguments were: '$second_last_arg' '$last_arg'"
        return 1
    end

    echo "→ Running: git clone $clone_args $repo_url $target_dir"
    if test -n "$target_dir"
        git clone $clone_args $repo_url $target_dir
    else
        git clone $clone_args $repo_url
    end
    if test $status -ne 0
        echo "Clone failed :("
        return 1
    end

    # Decide which directory to cd into
    set -l cd_dir
    if test -n "$target_dir"
        set cd_dir $target_dir
    else
        # Extract repo name from URL (remove .git, take last part)
        set cd_dir (string replace -r '.*://' '' $repo_url)
        set cd_dir (string replace -r '^.*/' '' $cd_dir)
        set cd_dir (string replace -r '\.git$' '' $cd_dir)
    end

    if not test -d "$cd_dir"
        echo "Warning: Directory '$cd_dir' not found after clone"
        return 1
    end

    echo "→ Entering: $cd_dir"
    cd "$cd_dir"
    git branch --show-current 2>/dev/null || true
end

complete --command nvm --exclusive --long version --description "Print version"
complete --command nvm --exclusive --long help --description "Print help"
complete --command nvm --exclusive --long no-colors --description "Print without colors"

complete --command nvm --exclusive --condition __fish_use_subcommand --arguments install --description "Download and activate the specified Node version"
complete --command nvm --exclusive --condition __fish_use_subcommand --arguments uninstall --description "Uninstall a version"
complete --command nvm --exclusive --condition __fish_use_subcommand --arguments use --description "Activate a version in the current shell"
complete --command nvm --exclusive --condition __fish_use_subcommand --arguments list --description "List installed versions"
complete --command nvm --exclusive --condition __fish_use_subcommand --arguments list-remote --description "List versions available to install matching optional regex"
complete --command nvm --exclusive --condition __fish_use_subcommand --arguments current --description "Print the currently-active version"
complete --command nvm --exclusive --condition "__fish_seen_subcommand_from install" --arguments "(command grep -Eo '(v[12][0-9]\.[0-9]{,2}\.[0-9]{,2})' ~/.cache/npm-ls-remote)"
complete --command nvm --exclusive --condition "__fish_seen_subcommand_from use" --arguments "(nvm ls --no-colors | sed -e '/^[[:alpha:]]/d' -e 's/\s//g' -e 's/\->//g' -e 's/\*//g')"
complete --command nvm --exclusive --condition "__fish_seen_subcommand_from uninstall" --arguments "(nvm ls --no-colors | sed -e '/^[[:alpha:]]/d' -e 's/\s//g' -e 's/\->//g' -e 's/\*//g')"
complete --command nvm --exclusive --condition "__fish_seen_subcommand_from use uninstall" --arguments "(
    set --query nvm_default_version && echo default
)"

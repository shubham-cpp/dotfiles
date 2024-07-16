complete -c ya -n "__fish_use_subcommand" -s h -l help -d 'Print help'
complete -c ya -n "__fish_use_subcommand" -s V -l version -d 'Print version'
complete -c ya -n "__fish_use_subcommand" -f -a "pub" -d 'Publish a message to remote instance(s)'
complete -c ya -n "__fish_use_subcommand" -f -a "pub-static" -d 'Publish a static message to all remote instances'
complete -c ya -n "__fish_use_subcommand" -f -a "help" -d 'Print this message or the help of the given subcommand(s)'
complete -c ya -n "__fish_seen_subcommand_from pub" -l str -d 'Send the message with a string body' -r
complete -c ya -n "__fish_seen_subcommand_from pub" -l json -d 'Send the message with a JSON body' -r
complete -c ya -n "__fish_seen_subcommand_from pub" -s h -l help -d 'Print help'
complete -c ya -n "__fish_seen_subcommand_from pub" -s V -l version -d 'Print version'
complete -c ya -n "__fish_seen_subcommand_from pub-static" -l str -d 'Send the message with a string body' -r
complete -c ya -n "__fish_seen_subcommand_from pub-static" -l json -d 'Send the message with a JSON body' -r
complete -c ya -n "__fish_seen_subcommand_from pub-static" -s h -l help -d 'Print help'
complete -c ya -n "__fish_seen_subcommand_from pub-static" -s V -l version -d 'Print version'
complete -c ya -n "__fish_seen_subcommand_from help; and not __fish_seen_subcommand_from pub; and not __fish_seen_subcommand_from pub-static; and not __fish_seen_subcommand_from help" -f -a "pub" -d 'Publish a message to remote instance(s)'
complete -c ya -n "__fish_seen_subcommand_from help; and not __fish_seen_subcommand_from pub; and not __fish_seen_subcommand_from pub-static; and not __fish_seen_subcommand_from help" -f -a "pub-static" -d 'Publish a static message to all remote instances'
complete -c ya -n "__fish_seen_subcommand_from help; and not __fish_seen_subcommand_from pub; and not __fish_seen_subcommand_from pub-static; and not __fish_seen_subcommand_from help" -f -a "help" -d 'Print this message or the help of the given subcommand(s)'

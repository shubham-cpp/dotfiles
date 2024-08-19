# Print an optspec for argparse to handle cmd's options that are independent of any subcommand.
function __fish_ya_global_optspecs
	string join \n V/version h/help
end

function __fish_ya_needs_command
	# Figure out if the current invocation already has a command.
	set -l cmd (commandline -opc)
	set -e cmd[1]
	argparse -s (__fish_ya_global_optspecs) -- $cmd 2>/dev/null
	or return
	if set -q argv[1]
		# Also print the command, so this can be used to figure out what it is.
		echo $argv[1]
		return 1
	end
	return 0
end

function __fish_ya_using_subcommand
	set -l cmd (__fish_ya_needs_command)
	test -z "$cmd"
	and return 1
	contains -- $cmd[1] $argv
end

complete -c ya -n "__fish_ya_needs_command" -s V -l version -d 'Print version'
complete -c ya -n "__fish_ya_needs_command" -s h -l help -d 'Print help'
complete -c ya -n "__fish_ya_needs_command" -f -a "pub" -d 'Publish a message to the current instance'
complete -c ya -n "__fish_ya_needs_command" -f -a "pub-to" -d 'Publish a message to the specified instance'
complete -c ya -n "__fish_ya_needs_command" -f -a "sub" -d 'Subscribe to messages from all remote instances'
complete -c ya -n "__fish_ya_needs_command" -f -a "pack" -d 'Manage packages'
complete -c ya -n "__fish_ya_needs_command" -f -a "help" -d 'Print this message or the help of the given subcommand(s)'
complete -c ya -n "__fish_ya_using_subcommand pub" -l str -d 'Send the message with a string body' -r
complete -c ya -n "__fish_ya_using_subcommand pub" -l json -d 'Send the message with a JSON body' -r
complete -c ya -n "__fish_ya_using_subcommand pub" -l list -d 'Send the message as string of list' -r
complete -c ya -n "__fish_ya_using_subcommand pub" -s h -l help -d 'Print help'
complete -c ya -n "__fish_ya_using_subcommand pub-to" -l str -d 'Send the message with a string body' -r
complete -c ya -n "__fish_ya_using_subcommand pub-to" -l json -d 'Send the message with a JSON body' -r
complete -c ya -n "__fish_ya_using_subcommand pub-to" -l list -d 'Send the message as string of list' -r
complete -c ya -n "__fish_ya_using_subcommand pub-to" -s h -l help -d 'Print help'
complete -c ya -n "__fish_ya_using_subcommand sub" -s h -l help -d 'Print help'
complete -c ya -n "__fish_ya_using_subcommand pack" -s a -l add -d 'Add a package' -r
complete -c ya -n "__fish_ya_using_subcommand pack" -s i -l install -d 'Install all packages'
complete -c ya -n "__fish_ya_using_subcommand pack" -s l -l list -d 'List all packages'
complete -c ya -n "__fish_ya_using_subcommand pack" -s u -l upgrade -d 'Upgrade all packages'
complete -c ya -n "__fish_ya_using_subcommand pack" -s h -l help -d 'Print help'
complete -c ya -n "__fish_ya_using_subcommand help; and not __fish_seen_subcommand_from pub pub-to sub pack help" -f -a "pub" -d 'Publish a message to the current instance'
complete -c ya -n "__fish_ya_using_subcommand help; and not __fish_seen_subcommand_from pub pub-to sub pack help" -f -a "pub-to" -d 'Publish a message to the specified instance'
complete -c ya -n "__fish_ya_using_subcommand help; and not __fish_seen_subcommand_from pub pub-to sub pack help" -f -a "sub" -d 'Subscribe to messages from all remote instances'
complete -c ya -n "__fish_ya_using_subcommand help; and not __fish_seen_subcommand_from pub pub-to sub pack help" -f -a "pack" -d 'Manage packages'
complete -c ya -n "__fish_ya_using_subcommand help; and not __fish_seen_subcommand_from pub pub-to sub pack help" -f -a "help" -d 'Print this message or the help of the given subcommand(s)'

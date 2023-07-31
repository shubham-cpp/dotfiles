function mkd -d "Create a New directory and cd into it"
    [ -n "$argv[1]" ]; and mkdir -p "$argv[1]"; and cd "$argv[1]"; or echo "Please provide a valid directory name"
end

function which -d "Print alias for program or print location of program"
    if [ -n "$argv[1]" ]
        functions "$argv[1]"; or /bin/which "$argv[1]"
    else
        echo "Please provide a valid program name"
    end
end
# function jnb -d "Start jupyter lab in background"
#     nohup jupyter lab &> /dev/null &
#     sleep 2
#     jupyter lab list | sed '/running servers/d'
# end

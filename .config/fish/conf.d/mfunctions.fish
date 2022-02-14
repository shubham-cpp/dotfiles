function mkd -d "Create a New directory and cd into it"
    [ -n "$argv[1]" ]; and mkdir -p "$argv[1]"; and cd "$argv[1]"; or echo "Please provide a valid directory name"
end

# function jnb -d "Start jupyter lab in background"
#     nohup jupyter lab &> /dev/null &
#     sleep 2
#     jupyter lab list | sed '/running servers/d'
# end

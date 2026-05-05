procId=$(ps -e | grep 'tmux.*server' | sed -E 's/([0-9]+).*/\1/')
kill $procId

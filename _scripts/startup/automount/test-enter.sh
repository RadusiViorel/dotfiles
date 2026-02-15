#!/bin/bash

echo "Press Enter to test, or 'q' to quit"
while true; do
    read -rsn1 key < /dev/tty
    
    if [[ "$key" == "q" ]]; then
        echo "Quit"
        break
    elif [[ "$key" == $'\n' ]]; then
        echo "Enter key detected!"
    else
        printf "Key pressed: %q\n" "$key"
    fi
done

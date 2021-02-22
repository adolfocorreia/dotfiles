function fish_greeting
    fortune
end

function fish_user_key_bindings
    fish_vi_key_bindings
    bind -M insert \cf forward-char
end

alias bat=batcat
alias subl="flatpak run com.sublimetext.three"

# >>> conda initialize >>>
# !! Contents within this block are managed by 'conda init' !!
eval /opt/miniconda/bin/conda "shell.fish" "hook" $argv | source
# <<< conda initialize <<<

starship init fish | source


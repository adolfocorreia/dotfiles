set -U fish_greeting

function fish_user_key_bindings
    fish_vi_key_bindings

    # Use Ctrl+Space to autocomplete fish suggestions
    # References: https://fishshell.com/docs/current/cmds/bind.html
    #             https://fishshell.com/docs/current/cmds/fish_key_reader.html
    bind -M insert -k nul accept-autosuggestion
end

set EDITOR nvim
set fzf_fd_opts --hidden --exclude .git --exclude .cache

alias ls=exa
alias open=xdg-open
alias qutebrowser="flatpak run org.qutebrowser.qutebrowser"

# >>> conda initialize >>>
# !! Contents within this block are managed by 'conda init' !!
eval /opt/miniconda/bin/conda "shell.fish" "hook" $argv | source
# <<< conda initialize <<<

starship init fish | source


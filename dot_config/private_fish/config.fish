function fish_user_key_bindings
    # Add emacs bindings to vi insert mode and then load vi-mode without resetting previous bindings
    # Reference: https://fishshell.com/docs/current/interactive.html#vi-mode-commands
    # Note: use Alt+E to open current command line in editor
    fish_default_key_bindings -M insert
    fish_vi_key_bindings --no-erase

    # Use Ctrl+Space to autocomplete fish suggestions
    # References: https://fishshell.com/docs/current/cmds/bind.html
    #             https://fishshell.com/docs/current/cmds/fish_key_reader.html
    bind -M insert -k nul accept-autosuggestion
end

function fish_right_prompt
    date +"[%H:%M:%S]"
end

# Commands to run in interactive sessions can go here
if status is-interactive

    # Disable fish greeting message
    set -U fish_greeting

    # Set environment variables
    set EDITOR nvim
    set fzf_fd_opts --hidden --exclude .git --exclude .cache

    # Set aliases
    alias ls=exa
    alias open=xdg-open

    # Load conda
    eval /opt/miniconda/bin/conda "shell.fish" "hook" $argv | source

    # Load starship
    starship init fish | source
end

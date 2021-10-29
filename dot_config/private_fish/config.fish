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

# Commands to run in interactive sessions can go here
if status is-interactive

    # Disable fish greeting message
    set -U fish_greeting

    # Set environment variables
    set EDITOR nvim
    set fzf_fd_opts --hidden --exclude .git --exclude .cache

    # Set aliases
    alias vi=nvim
    alias ls=exa
    alias open=xdg-open

    # Load conda
    # Reference: https://stackoverflow.com/questions/34280113/add-conda-to-path-in-fish
    if type -q /opt/miniconda3/bin/conda
        eval /opt/miniconda3/bin/conda "shell.fish" "hook" $argv | source
    end

    # Load starship
    starship init fish | source
end

function fish_user_key_bindings
    # Add emacs bindings to vi insert mode and then load vi-mode without resetting previous bindings
    # Reference: https://fishshell.com/docs/current/interactive.html#vi-mode-commands
    # Note: use Alt+E to open current command line in editor
    fish_default_key_bindings -M insert
    fish_vi_key_bindings --no-erase

    # Key binding references:
    # - https://fishshell.com/docs/current/cmds/bind.html
    # - https://fishshell.com/docs/current/cmds/fish_key_reader.html
    # - https://fishshell.com/docs/current/interactive.html#autosuggestions
end

# Commands to run in interactive sessions can go here
if status is-interactive

    # Disable fish greeting message
    set -U fish_greeting

    # Set environment variables
    set EDITOR nvim

    # Set aliases
    alias ls=exa
    alias vi=nvim
    alias open=xdg-open

    # Load conda
    # Reference: https://stackoverflow.com/questions/34280113/add-conda-to-path-in-fish
    if type -q /opt/miniconda3/bin/conda
        /opt/miniconda3/bin/conda "shell.fish" "hook" $argv | source
    end

    # Load pyenv
    if type -q pyenv
        pyenv init - | source
    end

    # Load starship
    starship init fish | source
end
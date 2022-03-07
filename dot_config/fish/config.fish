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

    # Set cursors for vi modes
    # Reference: https://fishshell.com/docs/current/interactive.html#vi-mode-commands
    set fish_cursor_default     block      blink
    set fish_cursor_insert      line       blink
    set fish_cursor_visual      block      blink
    set fish_cursor_replace     underscore blink
    set fish_cursor_replace_one underscore blink
    if set -q TMUX
        set fish_vi_force_cursor
    end

    # Set aliases
    alias ls=exa
    alias vi=nvim

    # Load conda
    # Reference: https://stackoverflow.com/questions/34280113/add-conda-to-path-in-fish
    if type -q /opt/miniconda3/bin/conda
        /opt/miniconda3/bin/conda "shell.fish" "hook" $argv | source
    end

    # Load pyenv
    if type -q pyenv
        pyenv init - | source
    end

    # Load direnv
    if type -q direnv
        direnv hook fish | source
    end

    # Load starship
    starship init fish | source
end

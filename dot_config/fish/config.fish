function bind_bang
    switch (commandline -t)[-1]
        case "!"
            commandline -t -- $history[1]
            commandline -f repaint
        case "*"
            commandline -i !
    end
end

function bind_dollar
    switch (commandline -t)[-1]
        case "!"
            commandline -f backward-delete-char history-token-search-backward
        case "*"
            commandline -i '$'
    end
end

function fish_user_key_bindings
    # Add emacs bindings to vi insert mode and then load vi-mode without resetting previous bindings
    # Reference: https://fishshell.com/docs/current/interactive.html#vi-mode-commands
    # Note: use Alt+E to open current command line in editor
    fish_default_key_bindings -M insert
    fish_vi_key_bindings --no-erase

    # Bind '!!' and '!$' as bash-style command substitution
    # Reference: https://github.com/fish-shell/fish-shell/wiki/Bash-Style-Command-Substitution-and-Chaining-(!!-!$)
    bind -M insert '!' bind_bang
    bind -M insert '$' bind_dollar

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
    alias ll="exa -l"
    alias la="exa -la"

    alias emacs="emacs -nw"
    alias vi="nvim --clean"

    # Sponge plugin settings
    set sponge_purge_only_on_exit true

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

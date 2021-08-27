function fish_user_key_bindings
    fish_vi_key_bindings

    # Use Ctrl+Space to autocomplete fish suggestions
    # References: https://fishshell.com/docs/current/cmds/bind.html
    #             https://fishshell.com/docs/current/cmds/fish_key_reader.html
    bind -M insert -k nul accept-autosuggestion
end

if status is-interactive
    # Commands to run in interactive sessions can go here

    set -U fish_greeting

    set EDITOR nvim
    set fzf_fd_opts --hidden --exclude .git --exclude .cache

    alias ls=exa
    alias open=xdg-open

    # conda
    eval /opt/miniconda/bin/conda "shell.fish" "hook" $argv | source

    starship init fish | source
end

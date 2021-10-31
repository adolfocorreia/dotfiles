# References:
# - https://github.com/PatrickF1/fzf.fish
# - https://github.com/PatrickF1/fzf.fish/blob/main/functions/_fzf_wrapper.fish
# - https://github.com/junegunn/fzf/wiki/Color-schemes

set --export FZF_DEFAULT_OPTS '
		--cycle
		--layout=reverse
		--border
		--height=90%
		--preview-window=wrap
		--marker="*"
		--color fg:#D8DEE9,bg:#2E3440,hl:#A3BE8C
		--color fg+:#D8DEE9,bg+:#434C5E,hl+:#A3BE8C
		--color pointer:#BF616A,info:#4C566A,spinner:#4C566A
		--color header:#4C566A,prompt:#81A1C1,marker:#EBCB8B
'

set fzf_fd_opts --hidden --exclude .git --exclude .cache


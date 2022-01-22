# References:
# - https://github.com/PatrickF1/fzf.fish
# - https://github.com/PatrickF1/fzf.fish/blob/main/functions/_fzf_wrapper.fish
# - https://github.com/junegunn/fzf/wiki/Color-schemes

# Nord theme:
# set --export FZF_DEFAULT_OPTS '
# 		--cycle
# 		--layout=reverse
# 		--border
# 		--height=90%
# 		--preview-window=wrap
# 		--marker="*"
# 		--color fg:#D8DEE9,bg:#2E3440,hl:#A3BE8C
# 		--color fg+:#D8DEE9,bg+:#434C5E,hl+:#A3BE8C
# 		--color pointer:#BF616A,info:#4C566A,spinner:#4C566A
# 		--color header:#4C566A,prompt:#81A1C1,marker:#EBCB8B
# '

# Tokyo Night theme:
set --export FZF_DEFAULT_OPTS '
		--cycle
		--layout=reverse
		--border
		--height=90%
		--preview-window=wrap
		--marker="*"
		--color fg:#c0caf5,bg:#24283b,hl:#9ece6a
		--color fg+:#c0caf5,bg+:#414868,hl+:#9ece6a
		--color pointer:#f7768e,info:#a9b1d6,spinner:#a9b1d6
		--color header:#a9b1d6,prompt:#7aa2f7,marker:#e0af68
'

set fzf_fd_opts --hidden --exclude .git --exclude .cache


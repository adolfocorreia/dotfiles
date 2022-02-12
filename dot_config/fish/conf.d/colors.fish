# References:
# - https://www.reddit.com/r/fishshell/comments/eri2sk/different_fish_shell_themes_on_install/
# - https://fishshell.com/docs/current/tutorial.html#universal-variables
# - https://github.com/fish-shell/fish-shell/tree/master/share/tools/web_config/themes
# - https://github.com/folke/tokyonight.nvim/tree/main/extras


# Nord theme:
# set -U fish_color_normal normal
# set -U fish_color_command 81a1c1
# set -U fish_color_quote a3be8c
# set -U fish_color_redirection b48ead
# set -U fish_color_end 88c0d0
# set -U fish_color_error ebcb8b
# set -U fish_color_param eceff4
# set -U fish_color_comment 434c5e
# set -U fish_color_match --background=brblue
# set -U fish_color_selection white --bold --background=brblack
# set -U fish_color_search_match bryellow --background=brblack
# set -U fish_color_history_current --bold
# set -U fish_color_operator 00a6b2
# set -U fish_color_escape 00a6b2
# set -U fish_color_cwd green
# set -U fish_color_cwd_root red
# set -U fish_color_valid_path --underline
# set -U fish_color_autosuggestion 4c566a
# set -U fish_color_user brgreen
# set -U fish_color_host normal
# set -U fish_color_cancel -r
# set -U fish_pager_color_completion normal
# set -U fish_pager_color_prefix normal --bold --underline
# set -U fish_pager_color_description B3A06D yellow
# set -U fish_pager_color_progress brwhite --background=cyan


# TokyoNight theme:
set -l foreground c0caf5
set -l selection 364A82
set -l comment 565f89
set -l red f7768e
set -l orange ff9e64
set -l yellow e0af68
set -l green 9ece6a
set -l purple 9d7cd8
set -l cyan 7dcfff
set -l pink bb9af7

set -U fish_color_normal $foreground
set -U fish_color_command $cyan
set -U fish_color_keyword $pink
set -U fish_color_quote $yellow
set -U fish_color_redirection $foreground
set -U fish_color_end $orange
set -U fish_color_error $red
set -U fish_color_param $purple
set -U fish_color_comment $comment
set -U fish_color_selection --background=$selection
set -U fish_color_search_match --background=$selection
set -U fish_color_operator $green
set -U fish_color_escape $pink
set -U fish_color_autosuggestion $comment
set -U fish_pager_color_progress $comment
set -U fish_pager_color_prefix $cyan
set -U fish_pager_color_completion $foreground
set -U fish_pager_color_description $comment


# Reference: https://github.com/chubin/wttr.in
function wttr --description "Prints weather information from wttr.in"
    set --local options 'h/help' 'v/version=!_validate_int --min 1 --max 3'

    argparse $options -- $argv
    or return

    if set --query _flag_help
        curl "wttr.in/:help"
        return 0
    end

    set --query _flag_version; or set --local _flag_version 1

    set --local location (string replace ' ' '+' (string join '+' $argv))

    set --local url (string join '' v $_flag_version .wttr.in/ $location '?F')

    curl $url
end

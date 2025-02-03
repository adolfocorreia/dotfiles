# Reference: https://github.com/chubin/wttr.in
function wttr --description "Prints weather information from wttr.in"
    set spec 'h/help' 'v/version=!_validate_int --min 1 --max 3' 'o/options=?'

    argparse $spec -- $argv
    or return 1

    if set --query _flag_help
        curl "wttr.in/:help"
        return 0
    end

    set --query _flag_version; or set _flag_version 1

    set --query _flag_options; or set _flag_options 'F'

    if test (count $argv) -gt 0
        set location $argv
    else
        read location < "$XDG_CONFIG_HOME/wttr/default-city.txt"
    end
    set location (string replace --all ' ' '+' (string join '+' $location))

    set url (string join '' 'v' $_flag_version '.wttr.in/' $location '?' $_flag_options)

    curl $url
end

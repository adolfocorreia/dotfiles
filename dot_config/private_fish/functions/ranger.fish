# Reference: https://github.com/ranger/ranger/wiki/Integration-with-other-programs#changing-directories
function ranger
	set tempfile (mktemp -t tmp.XXXXXX)
	set command_argument "map Q chain shell echo %d > $tempfile; quitall"
	command ranger --cmd="$command_argument" $argv
	if test -s $tempfile
		set ranger_pwd (cat $tempfile)
		if test -n $ranger_pwd -a -d $ranger_pwd
			cd -- $ranger_pwd
		end
	end

	command rm -f -- $tempfile
end


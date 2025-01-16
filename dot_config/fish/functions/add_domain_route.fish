function add_domain_route --description 'Add direct routes to addresses pointed by domain'
    set default $(ip route | grep default | awk '{print $3}')
    test -n $(string match --regex '\d+\.\d+\.\d+\.\d+' $default); or return

    set domains $argv[1]
    test -n $domains; or return

    set addresses

    while test $(count $domains) -gt 0
        set domain $domains[1]
        set --erase domains[1]

        for item in $(dig +short $domain)
            if string match --quiet --regex '\d+\.\d+\.\d+\.\d+' $item
                set --append addresses $item
            else
                set --append domains $item
            end
        end
    end

    for address in $(string collect $addresses | sort | uniq)
        if test $(ip route | grep -c $address) -eq 0
            sudo ip route add $address via $default
            echo "Added route to: $address"
        end
    end
end

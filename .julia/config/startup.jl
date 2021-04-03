atreplinit() do repl
    try
        @eval using OhMyREPL

        @async begin
            # Reinstall keybindings
            # Reference: https://github.com/KristofferC/OhMyREPL.jl/issues/166
            sleep(1)
            OhMyREPL.Prompt.insert_keybindings()
        end
    catch e
        @warn "error while importing OhMyREPL" e
    end
end

# Set the nul device as the default GR (Plots.jl backend) output
# (useful when you don't need the plot window, e.g. when plotting to files)
ENV["GKSwstype"] = "nul"


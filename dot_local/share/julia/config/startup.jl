import REPL
atreplinit() do repl
    # Enable IPython-like numbered prompt
    if !isdefined(repl, :interface)
        repl.interface = REPL.setup_interface(repl)
    end
    REPL.numbered_prompt!(repl)

    # Load OhMyREPL
    try
        @eval import OhMyREPL
    catch e
        @eval import Pkg
        Pkg.add("OhMyREPL")
        @eval import OhMyREPL
    end
    @async begin
        # Autocomplete brackets breaks copying multiline statements to the REPL
        OhMyREPL.enable_autocomplete_brackets(false)

        # Reinstall keybindings
        # Reference: https://github.com/KristofferC/OhMyREPL.jl/issues/166
        sleep(1)
        OhMyREPL.Prompt.insert_keybindings()
    end
end

# Set the nul device as the default GR (Plots.jl backend) output
# (useful when you don't need the plot window, e.g. when plotting to files)
# References:
# - https://discourse.julialang.org/t/deactivate-plot-display-to-avoid-need-for-x-server
# - https://gr-framework.org/workstations.html
# ENV["GKSwstype"] = "nul"

# Always activate project in current directory
import Pkg
if isfile("Project.toml") && isfile("Manifest.toml")
    Pkg.activate(".")
end


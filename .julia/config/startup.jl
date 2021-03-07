atreplinit() do repl
    try
        @eval using OhMyREPL
    catch e
        @warn "error while importing OhMyREPL" e
    end
end

# Set the nul device as the default GR (Plots.jl backend) output
# (useful when you don't need the plot window, e.g. when plotting to files)
ENV["GKSwstype"] = "nul"


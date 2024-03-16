# Code editors and IDEs
AddPackage emacs-nativecomp # The extensible, customizable, self-documenting real-time display editor with native compilation enabled
AddPackage gvim # Vi Improved, a highly configurable, improved version of the vi text editor (with advanced features, such as a GUI)
AddPackage pycharm-community-edition # Python IDE for Professional Developers
AddPackage --foreign rstudio-desktop-bin # An integrated development environment (IDE) for R (binary from RStudio official repository)
AddPackage --foreign sublime-merge # Meet a new Git Client, from the makers of Sublime Text
AddPackage --foreign sublime-text-4 # Sophisticated text editor for code, html and prose - stable build
AddPackage --foreign visual-studio-code-bin # Visual Studio Code (vscode): Editor for building and debugging modern web and cloud applications (official binary version)

IgnorePath '/usr/share/*vim/*/doc/tags'

# Development tools
AddPackage cmake # A cross-platform open-source make system
AddPackage ctags # Generates an index file of language objects found in source files
AddPackage direnv # A shell extension that manages your environment
AddPackage meld # Compare files, directories and working copies
AddPackage pre-commit # A framework for managing and maintaining multi-language pre-commit hooks
AddPackage vint # Lint tool for Vim script Language
AddPackage --foreign dasel # Select, put and delete data from JSON, TOML, YAML, XML and CSV files with a single tool

# Shell
AddPackage shellcheck # Shell script analysis tool
AddPackage shfmt # Format shell programs

# Python
AddPackage ipython # Enhanced Interactive Python shell
AddPackage pyenv # Easily switch between multiple versions of Python
AddPackage pyright # Type checker for the Python language
AddPackage python-black # Uncompromising Python code formatter
AddPackage python-debugpy # An implementation of the Debug Adapter Protocol for Python
AddPackage python-isort # A Python utility / library to sort Python imports
AddPackage python-lsp-server # Fork of the python-language-server project, maintained by the Spyder IDE team and the community
AddPackage python-matplotlib # A python plotting library, making publication quality plots
AddPackage python-poetry # Python dependency management and packaging made easy
AddPackage python-pycodestyle # Python style guide checker
AddPackage python-pydocstyle # Docstring style checker
AddPackage python-pyflakes # A lint-like tool for Python to identify common errors quickly without executing code
AddPackage python-pylint # Analyzes Python code looking for bugs and signs of poor quality
AddPackage python-rope # Refactoring library
AddPackage python-whatthepatch # A Python patch parsing library
AddPackage ruff # An extremely fast Python linter, written in Rust
AddPackage tk # A windowing toolkit for use with tcl
AddPackage yapf # Python style guide checker

# Julia
AddPackage --foreign julia-bin # High-level, high-performance, dynamic programming language - official binaries

# R
AddPackage blas-openblas # An optimized BLAS library based on GotoBLAS2 1.13 BSD (Provides BLAS/CBLAS/LAPACK/LAPACKE system-wide)
AddPackage gcc-fortran # Fortran front-end for GCC
AddPackage r # Language and environment for statistical computing and graphics

# Lua
AddPackage selene # Blazing-fast modern Lua linter written in Rust
AddPackage stylua # Code formatter for Lua

# Haskell
AddPackage haskell-language-server # LSP server for GHC

# Rust
AddPackage rustup # The Rust toolchain installer

# Go
AddPackage go # Core compiler tools for the Go programming language

# JavaScript
AddPackage npm # A package manager for JavaScript

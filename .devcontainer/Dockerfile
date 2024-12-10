# Use Ubuntu 22.04 as the base image
FROM ubuntu:22.04

# Install essential tools and dependencies for Haskell and development
RUN apt-get update && \
    apt-get install -y \
    curl \
    wget \
    git \
    build-essential \
    libgmp-dev && \
    apt-get clean && \
    rm -rf /var/lib/apt/lists/*

# Install GHCup (Haskell Toolchain Manager), GHC (specific version), and Cabal
RUN curl -sSL https://get-ghcup.haskell.org | bash -s -- --non-interactive && \
    /root/.ghcup/bin/ghcup install ghc latest && \
    /root/.ghcup/bin/ghcup install cabal latest && \
    /root/.ghcup/bin/ghcup set ghc latest && \
    /root/.ghcup/bin/ghcup set cabal latest

# Set PATH to include GHCup binaries explicitly
ENV PATH="/root/.ghcup/bin:$PATH"

# Install ormolu
RUN cabal install ormolu

# ╔════════════════════════════════════════════════════════════════════╗
#                              🛈 COMMANDS                             
#   Custom commands defined in this Dockerfile for ease of use.        
#   These commands simplify repetitive tasks or add new functionality. 
# ╚════════════════════════════════════════════════════════════════════╝


# -- Command `r`: Quickly run cabal for a specific day and part --
RUN echo -e '\n# Alias for running Advent of Code\nr() { cabal run "day$1" "$2"; }' >> /root/.bashrc

# -- Command `new`: Creates a new day folder for Advent of Code --
RUN echo 'new() {' >> /root/.bashrc && \
    echo '    if [ $# -ne 1 ]; then' >> /root/.bashrc && \
    echo '        echo "Usage: new <day number (e.g., 04)>"' >> /root/.bashrc && \
    echo '        return 1' >> /root/.bashrc && \
    echo '    fi' >> /root/.bashrc && \
    echo '' >> /root/.bashrc && \
    echo '    DAY_NUM=$1' >> /root/.bashrc && \
    echo '    DAY_FOLDER="day${DAY_NUM}"' >> /root/.bashrc && \
    echo '    TEMPLATE_FOLDER="dayXX"' >> /root/.bashrc && \
    echo '' >> /root/.bashrc && \
    echo '    if [ -d "$TEMPLATE_FOLDER" ]; then' >> /root/.bashrc && \
    echo '        cp -r "$TEMPLATE_FOLDER" "$DAY_FOLDER"' >> /root/.bashrc && \
    echo '        echo "Copied $TEMPLATE_FOLDER to $DAY_FOLDER"' >> /root/.bashrc && \
    echo '    else' >> /root/.bashrc && \
    echo '        echo "Template folder $TEMPLATE_FOLDER does not exist."' >> /root/.bashrc && \
    echo '        return 1' >> /root/.bashrc && \
    echo '    fi' >> /root/.bashrc && \
    echo '' >> /root/.bashrc && \
    echo '    find "$DAY_FOLDER" -type f -exec sed -i "s|dayXX|${DAY_FOLDER}|g" {} +' >> /root/.bashrc && \
    echo '    echo "Replaced all occurrences of dayXX with $DAY_FOLDER in $DAY_FOLDER files."' >> /root/.bashrc && \
    echo '' >> /root/.bashrc && \
    echo '    CABAL_FILE="adventofcode2024.cabal"' >> /root/.bashrc && \
    echo '    if [ -f "$CABAL_FILE" ]; then' >> /root/.bashrc && \
    echo '        cat <<EOL >> "$CABAL_FILE"' >> /root/.bashrc && \
    echo '' >> /root/.bashrc && \
    echo 'executable $DAY_FOLDER' >> /root/.bashrc && \
    echo '    import:           warnings' >> /root/.bashrc && \
    echo '    main-is:          main.hs' >> /root/.bashrc && \
    echo '    hs-source-dirs:   $DAY_FOLDER' >> /root/.bashrc && \
    echo '    build-depends:    base >= 4.17 && < 4.21' >> /root/.bashrc && \
    echo '    other-modules:    Part1, Part2' >> /root/.bashrc && \
    echo '    default-language: Haskell2010' >> /root/.bashrc && \
    echo 'EOL' >> /root/.bashrc && \
    echo '        echo "Updated $CABAL_FILE with executable $DAY_FOLDER"' >> /root/.bashrc && \
    echo '    else' >> /root/.bashrc && \
    echo '        echo "Cabal file $CABAL_FILE not found."' >> /root/.bashrc && \
    echo '        return 1' >> /root/.bashrc && \
    echo '    fi' >> /root/.bashrc && \
    echo '}' >> /root/.bashrc

# Cleanup temporary files
RUN rm -rf /tmp/*

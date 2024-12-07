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

# Install GHCup (Haskell Toolchain Manager), GHC, and Cabal
RUN curl -sSL https://get-ghcup.haskell.org | bash -s -- --non-interactive && \
    /root/.ghcup/bin/ghcup install ghc latest && \
    /root/.ghcup/bin/ghcup install cabal latest && \
    /root/.ghcup/bin/ghcup set ghc latest && \
    /root/.ghcup/bin/ghcup set cabal latest

# Set PATH to include GHCup binaries explicitly
ENV PATH="/root/.ghcup/bin:$PATH"

# Cleanup
RUN rm -rf /tmp/*

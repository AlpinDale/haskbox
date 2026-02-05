FROM ubuntu:22.04

# Avoid interactive prompts
ENV DEBIAN_FRONTEND=noninteractive

# Install basic tools and GHC dependencies
RUN apt-get update && apt-get install -y \
    curl \
    git \
    build-essential \
    libffi-dev \
    libgmp-dev \
    zlib1g-dev \
    libnuma-dev \
    libncurses5-dev \
    libtinfo-dev \
    coreutils \
    && rm -rf /var/lib/apt/lists/*

# Install GHCup with the same versions as CI
RUN curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org | \
    BOOTSTRAP_HASKELL_NONINTERACTIVE=1 \
    BOOTSTRAP_HASKELL_GHC_VERSION=9.14.1 \
    BOOTSTRAP_HASKELL_CABAL_VERSION=3.16.1.0 \
    BOOTSTRAP_HASKELL_INSTALL_NO_STACK=1 \
    sh

# Add GHCup to PATH
ENV PATH="/root/.ghcup/bin:${PATH}"

# Set working directory
WORKDIR /workspace

# Default command
CMD ["/bin/bash"]

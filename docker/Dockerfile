FROM debian:trixie

LABEL maintainer="PIMacs"
LABEL description="PIMacs - Extended Doom Emacs configuration for testing"

ENV DEBIAN_FRONTEND=noninteractive
ENV LANG=C.UTF-8
ENV LC_ALL=C.UTF-8

# Mise configuration (following https://mise.jdx.dev/mise-cookbook/docker.html)
ENV MISE_DATA_DIR="/mise"
ENV MISE_CONFIG_DIR="/mise"
ENV MISE_CACHE_DIR="/mise/cache"
ENV MISE_INSTALL_PATH="/usr/local/bin/mise"
ENV MISE_YES=1
ENV MISE_TRUSTED_CONFIG_PATHS=/home/pimacs/.doom.d/modules/pimacs:/home/pimacs/host-home
# Add mise shims to PATH (before other paths for priority)
ENV PATH="/mise/shims:${PATH}"

# Install Emacs and dependencies
RUN apt-get update && apt-get install -y \
    emacs \
    build-essential \
    git \
    curl \
    ripgrep \
    fd-find \
    libtree-sitter-dev \
    fonts-cascadia-code \
    fonts-dejavu \
    fonts-hack \
    aspell \
    aspell-fr \
    aspell-en \
    && rm -rf /var/lib/apt/lists/*

# Install mise
RUN curl https://mise.run | sh

# Create mise directories with proper permissions
RUN mkdir -p /mise/cache && chmod -R 777 /mise

# Create non-root user
RUN useradd -m -s /bin/bash pimacs
USER pimacs
WORKDIR /home/pimacs

# Install Doom Emacs
RUN git clone --depth 1 https://github.com/doomemacs/doomemacs ~/.emacs.d \
    && ~/.emacs.d/bin/doom install --no-env --no-hooks

# Create Doom config directory
RUN mkdir -p /home/pimacs/.doom.d/modules

# Copy PIMacs Docker configuration files
COPY --chown=pimacs:pimacs my.el /home/pimacs/.doom.d/init.el
COPY --chown=pimacs:pimacs docker/config.el /home/pimacs/.doom.d/config.el
COPY --chown=pimacs:pimacs docker/packages.el /home/pimacs/.doom.d/packages.el

# Copy PIMacs modules
COPY --chown=pimacs:pimacs . /home/pimacs/.doom.d/modules/pimacs

# Run doom sync to install packages
RUN ~/.emacs.d/bin/doom sync

# Trust and install mise tools for language modules
RUN mise trust /home/pimacs/.doom.d/modules/pimacs/lang-go/mise.toml \
    && cd /home/pimacs/.doom.d/modules/pimacs/lang-go && mise install \
    && mise use -g go@latest \
    && mise use -g golangci-lint@latest \
    && mise reshim

# Pre-install tree-sitter grammars
# Create the cache directory first so grammars install to the correct location
RUN mkdir -p /home/pimacs/.emacs.d/.local/cache/tree-sitter && \
    emacs --batch \
    -l .doom.d/modules/pimacs/treesit/config.el \
    --eval "(pim-treesit-install-all-grammars)"

# Default command
CMD ["emacs"]

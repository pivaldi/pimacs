#!/bin/bash
#
# Run PIMacs in a Docker container with X11 support (Linux only)
#
# Usage:
#   ./docker/run.sh          # Run Emacs GUI with $HOME mounted
#   ./docker/run.sh -nw      # Run Emacs in terminal
#   ./docker/run.sh FILE     # Open a file
#

set -e

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
IMAGE_NAME="pimacs:latest"

touch ~/.projectile

cleanup() {
    xhost -local:docker 2>/dev/null || true
}

trap cleanup EXIT

# Check if image exists
if ! docker image inspect "$IMAGE_NAME" &>/dev/null; then
    echo "Image '$IMAGE_NAME' not found. Building..."
    (
        cd "$SCRIPT_DIR/.." || exit 1
        make docker-build
    )
fi

# Allow local Docker to access X11 display
xhost +local:docker 2>/dev/null || true

# Run Emacs in container with user's home mounted
docker run -it --rm \
    --name pimacs \
    -e DISPLAY="$DISPLAY" \
    -v /tmp/.X11-unix:/tmp/.X11-unix \
    -v "$HOME:/home/pimacs/host-home" \
    -w /home/pimacs/host-home \
    "$IMAGE_NAME" \
    emacs "$@"

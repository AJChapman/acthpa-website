#!/usr/bin/env bash

# Run this command from anywhere as deploy.sh.
# It assumes certain things about the environment:
#   1. deploy.sh is in the bin/ dir of the acthpa-website/ directory
#   2. There exists an folder at acthpa-website/../acthpa.org that is configured to be a WebDAV mount of the acthpa.org web server.
#   3. You have nix installed.

set -euo pipefail
set -x

# Get the location of this script
BIN_DIR=$(dirname "$(readlink -f "$0")")

# And of our repo
ACTHPA_DIR=$(readlink -f "${BIN_DIR}/..")
MOUNT=${MOUNT:-"${ACTHPA_DIR}/../acthpa.org"}
DEPLOY_TO=${DEPLOY_TO:-"${MOUNT}/public_html/"}

case "$(basename "$0")" in
    deploy.sh)
        # Make sure we're in the acthpa-website base dir
        cd "${ACTHPA_DIR}"

        # Make sure the repo is up to date
        git pull

        # Make sure the WebDAV directory is mounted
        mount "${MOUNT}" || true

        # Start this script running as nix-deploy.sh in a nix-shell
        nix-shell --run "${BIN_DIR}/nix-deploy.sh"
        ;;

    nix-deploy.sh)
        # Now that we're running inside nix-shell, build the site and deploy it
        cd "${ACTHPA_DIR}"
        cabal new-build scrape
        cabal new-exec scrape
        cabal new-build build-site

        # Remove any previous build
        rm -rf gen/ .shake/

        cabal new-exec build-site
        rsync -rvz gen/ "${DEPLOY_TO}"
        ;;

    *)
        echo "Unknown script name: $(basename "$0")"
        exit 1
        ;;

esac

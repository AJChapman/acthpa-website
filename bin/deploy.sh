#!/usr/bin/env bash

# Run this command from anywhere as deploy.sh.
# It assumes certain things about the environment:
#   1. deploy.sh is in the bin/ dir of the acthpa-website/ directory
#   2. The web server will serve the gen/ dir, which must be chowned :zulip (because this is how it is on the server)
#   3. You have nix installed.

set -euo pipefail
# set -x

# Get the location of this script
BIN_DIR=$(dirname "$(readlink -f "$0")")

# And of our repo
ACTHPA_DIR=$(readlink -f "${BIN_DIR}/..")

GEN_DIR=${ACTHPA_DIR}/gen
WEB_USER_GROUP=zulip

# Make sure we're in the acthpa-website base dir
cd "${ACTHPA_DIR}"

case "$(basename "$0")" in
    update.sh)
        # Make sure the repo is up to date
        git pull
        ;;

    scrape.sh)
        # Start this script running as nix-build.sh in a nix-shell
        nix-shell --run "${BIN_DIR}/nix-build.sh scrape"
        nix-shell --run "${BIN_DIR}/nix-exec.sh scrape"
        ;;

    build-deploy.sh)
        "${BIN_DIR}/build.sh"
        "${BIN_DIR}/deploy.sh"
        ;;

    update-build-deploy.sh)
        "${BIN_DIR}/update.sh"
        "${BIN_DIR}/build-deploy.sh"
        ;;

    scrape-build-deploy.sh)
        "${BIN_DIR}/scrape.sh"
        "${BIN_DIR}/build-deploy.sh"
        ;;

    update-scrape-build.sh)
        "${BIN_DIR}/update.sh"
        "${BIN_DIR}/scrape.sh"
        "${BIN_DIR}/build.sh"
        ;;

    update-scrape-build-deploy.sh)
        "${BIN_DIR}/update.sh"
        "${BIN_DIR}/scrape-build-deploy.sh"
        ;;

    build.sh)
        # Start this script running as nix-build.sh in a nix-shell
        nix-shell --run "${BIN_DIR}/nix-build.sh build-site"

        # Remove any previous build so we don't get caching / old data problems
        rm -rf gen/ .shake/

        nix-shell --run "${BIN_DIR}/nix-exec.sh build-site"
        ;;

    deploy.sh)
        chown -R :${WEB_USER_GROUP} ${GEN_DIR}
        ;;

    nix-exec.sh)
        TARGET="$1"
        cabal new-exec "$TARGET"
        ;;
        
    nix-build.sh)
        TARGET="$1"
        # Now that we're running inside nix-shell we can call cabal
        cabal new-build "$TARGET"
        ;;

    *)
        echo "Unknown script name: $(basename "$0")"
        exit 1
        ;;

esac

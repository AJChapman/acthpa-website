#!/usr/bin/env bash

set -euo pipefail

DEPLOY_TO=${DEPLOY_TO:-"acthpa.org:public_html/"}

# Get the location of this script
BIN_DIR=$(dirname "$(readlink -f "$0")")

# And of our repo
ACTHPA_DIR=$(readlink -f "${BIN_DIR}/..")

case "$(basename "$0")" in
    deploy.sh)
        # Make sure we're in the acthpa-website base dir
        cd "${ACTHPA_DIR}"

        # Make sure the repo is up to date
        git pull
        
        # Start this script running as nix-deploy.sh in a nix-shell
        nix-shell --run "${BIN_DIR}/nix-deploy.sh"
        ;;

    nix-deploy.sh)
        # Now that we're running inside nix-shell, build the site and deploy it
        cabal new-build scrape
        cabal new-exec scrape
        cabal new-build build-site
        cabal new-exec build-site
        rsync -rvz gen/ "${DEPLOY_TO}"
        ;;

    *)
        echo "Unknown script name: $(basename "$0")"
        exit 1
        ;;

esac

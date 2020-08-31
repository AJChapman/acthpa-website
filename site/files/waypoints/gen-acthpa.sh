#!/usr/bin/env bash

set -euo pipefail

# Generate ACTHPA.wpt
gpsbabel -i kml -f ACTHPA.kml -o compegps -F ACTHPA.wpt

# Get turnpoints within 50km of Mt Elliot Launch from the Victorian waypoints file,
# Add in all the ACTHPA turnpoints, output TPCACT2020.kml
gpsbabel -i kml -f TPCVictoria2020.kml -x 'radius,lat=-36.186035,lon=147.974553,distance=50K' -i kml -f ACTHPA.kml -o kml -F TPCACT2020.kml

# Convert TPCACT2020.kml to TPCACT2020.wpt
gpsbabel -i kml -f TPCACT2020.kml -o compegps -F TPCACT2020.wpt

#!/bin/bash
## This script works in the up-to-date alt3 repository
## Argument is league
source ~/alt3-environment.sh
cd "$ALT3_HOME"

league=$1
export league


cd docs/assets/leagues/"$league"
mkdir "$ALT3_CURRENT_SEASON"
mkdir "$ALT3_CURRENT_SEASON"/schedule-strengths

Rscript "$ALT3_SCRIPTS"/setup-teamnames.R $league $ALT3_CURRENT_SEASON

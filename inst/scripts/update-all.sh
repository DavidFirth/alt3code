#!/bin/bash
#
# Run this in $ALT3_HOME   ## Robustify this?
#
git pull origin master   ## This is redundant if called in a github action
                         ## so maybe test for that?
#
while IFS= read -r league
do
    nice -19 "$ALT3_SCRIPTS"/update.sh "$league"
done < "$ALT3_SCRIPTS"/leagues.txt
#
Rscript "$ALT3_SCRIPTS"/push.R

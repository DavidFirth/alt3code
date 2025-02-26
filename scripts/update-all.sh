#!/bin/bash
#
source ~/alt3-environment.sh
#
while IFS= read -r league
do
    nice -19 "$ALT3_SCRIPTS"/update.sh "$league"
done < "$ALT3_SCRIPTS"/leagues.txt
#

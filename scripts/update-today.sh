#!/bin/bash
#
while IFS= read -r league
do
    nice -19 "$ALT3_SCRIPTS"/update.sh "$league"
done < "$ALT3_HOME"/docs/assets/leagues/leagues-playing-today.txt
#

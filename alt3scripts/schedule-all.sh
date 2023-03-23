#!/bin/bash
rm -rf "$ALT3_SCRIPTS"/push-times-today.txt
while IFS= read -r league
do
    nice -19 "$ALT3_SCRIPTS"/schedule.R "$league"
done < "$ALT3_SCRIPTS"/leagues.txt
Rscript "$ALT3_SCRIPTS"/schedule-pushes.R
echo ''


### Add date as an argument here, so I can play with it? Maybe not.

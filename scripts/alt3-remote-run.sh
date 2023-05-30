#! /bin/bash
#
## For daily update of alt-3 locally, not in github actions
##
cd $ALT3_HOME
git pull origin master
cd $ALT3_CODE/..
git pull origin main
scripts/leagues-playing-today.R
if [ -e $ALT3_HOME/docs/assets/leagues/leagues-playing-today.txt ]
then
    $ALT3_CODE/../scripts/update-today.sh
    $ALT3_CODE/../scripts/push.R
else
    echo "nothing done"
fi



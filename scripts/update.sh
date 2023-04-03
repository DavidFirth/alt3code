#!/bin/bash
## This script works in the up-to-date alt3 repository
cd "$ALT3_HOME"

league=$1
export league

# notification="*$league*\n:large_blue_square: Starting an update run $(date +"%T")\n"
# Disabled the above, it's a distraction

# git pull origin master
# April 2021, try working without the automated pull, to avoid clashes

Rscript "$ALT3_SCRIPTS"/update.R $league $ALT3_CURRENT_SEASON

#add, commit and push files

git config --global user.name 'DavidFirth'
git config --global user.email 'DavidFirth@users.noreply.github.com'
gitstatus=$(git status --porcelain)

newresults=""
newresults=$(echo "$gitstatus" | grep "^??.*$league" | sed 's/?? //')

matches="No new"

if [ -n "$newresults" ]
then
    matches=$(echo "$newresults" | sed -e 's/.*results//' -e 's/.csv//')
fi

notification="$notification"":large_yellow_square: $matches matches completed"

if [ -z "$newresults" ]
then
    echo "no new results"
fi

if [ -n "$newresults" ]
then
    echo 'New results are in '$newresults
    git add "$newresults"
    git commit -m "Update $league after $matches" --only $(echo "$gitstatus" | grep "$league" | sed 's/.* //')
    echo "Done updating $league"
    notification="$notification\n:large_green_square: Commit $(date +"%T")"
fi

# echo "$notification"
echo "Sending notification to slack"

curl -X POST -H 'Content-type: application/json' --data \{\"text\":\""$notification"\"\} "$SLACK_URL"

echo "
"

exit 0 




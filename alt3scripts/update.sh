#!/bin/bash
league=$1
export league

notification="*$league*\n:large_blue_square: Starting an update run $(date +"%T")\n"

cd "$ALT3_HOME"

# git pull origin master
# April 2021, try working without the automated pull, to avoid clashes

Rscript "$ALT3_SCRIPTS"/update.R $league 2022

#add, commit and push files

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
    git add "$newresults"
    git commit -m "Update $league after $matches" --only $(echo "$gitstatus" | grep "$league" | sed 's/.* //')
    echo "Done updating $league"
    notification="$notification\n:large_green_square: Commit $(date +"%T")"
fi

echo "$notification"

curl -X POST -H 'Content-type: application/json' --data \{\"text\":\""$notification"\"\} "$SLACK_URL"

echo "
"

exit 0 



#!/bin/sh
source ~/alt3-environment.sh

notification="*$league*\n:large_blue_square: Pulling from GitHub $(date +"%T")\n"
curl -X POST -H 'Content-type: application/json' --data \{\"text\":\""$notification"\"\} "$SLACK_URL"

cd "$ALT3_HOME"
git pull origin master

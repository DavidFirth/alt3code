#!/usr/bin/env Rscript
## Usage: e.g.,
##   Rscript setup-teamnames.R spain-la-liga-primera 2025

args <-  commandArgs(trailingOnly = TRUE)
if (length(args) != 2) stop("two arguments needed, league and season")

league <- args[1]
season <- args[2]
lastseason <- as.numeric(season) - 1
message(league)

library(devtools)
load_all(Sys.getenv("ALT3_CODE"))

setwd(paste0(Sys.getenv("ALT3_HOME"), "/docs/assets/leagues"))

fetch_teams(league, season)
process_team_names(league, season)

newteams <- read.csv(paste0(league, "/", season, "/teams.csv"))
oldteams <- read.csv(paste0(league, "/", lastseason, "/namesOfTeams.csv"))

for (id in newteams $ fdo_id) {
    if (id %in% oldteams $ fdo_id){
        newteams[newteams $ fdo_id == id, "abbrev"] <- oldteams[oldteams $ fdo_id == id, "abbrev"]
        newteams[newteams $ fdo_id == id, "BBC_name"] <- oldteams[oldteams $ fdo_id == id, "BBC_name"]
    }
}

write.csv(newteams, file = paste0(league, "/", season, "/namesOfTeams.csv"))

cat("Now edit the new file 'namesOfTeams.csv' by hand\n")




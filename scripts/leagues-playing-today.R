#!/usr/bin/env Rscript
## Usage: e.g.,
##   Rscript leagues-playing-today.R
##
## This writes an edited version of leagues.txt, in the assets/leagues directory of alt3,
## whenever there are matches being played today in one or more leagues
#
leagues <- scan(paste0(Sys.getenv("ALT3_SCRIPTS"), "/leagues.txt"), what = character(0))
season <- Sys.getenv("ALT3_CURRENT_SEASON")
the_date <- if (length(args) == 1) Sys.Date() else args[2]
#the_date <- "2023-05-28"

library(devtools)
load_all(Sys.getenv("ALT3_CODE"))
setwd(paste0(Sys.getenv("ALT3_HOME"), "/docs/assets/leagues"))
if (file.exists("leagues-playing-today.txt")) {
    invisible(file.remove("leagues-playing-today.txt"))
}

leagues_playing_today <- leagues
names(leagues_playing_today) <- leagues

## Find out which leagues have matches scheduled today
for (league in leagues) {
schedule <- read.csv(paste(league, "/", season, "/latest.csv", sep = ""))
kickoffs_today <- schedule$UTCtime[schedule$date == the_date]
if (!(length(kickoffs_today) > 0)) is.na(leagues_playing_today[league]) <- TRUE
}


if (all(is.na(leagues_playing_today))) {
    notify_me <- slack
    notify_me(head = "Check for fixtures",
              body = ":large_purple_square: No matches today"
              )
    cat("\n")
} else {
    cat(na.omit(leagues_playing_today), sep = "\n", file = "leagues-playing-today.txt")
}



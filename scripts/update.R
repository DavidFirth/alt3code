#!/usr/bin/env Rscript
## Usage: e.g.,
##   Rscript update.R spain-la-liga-primera 2021

args <-  commandArgs(trailingOnly = TRUE)
if (length(args) != 2) stop("two arguments needed, league and season")

league <- args[1]
season <- args[2]
message(league)

library(devtools)
load_all(Sys.getenv("ALT3_CODE"))

notify_me <- slack

setwd(paste0(Sys.getenv("ALT3_HOME"), "/docs/assets/leagues"))

fetch_results(league = league, season = season)
results <- process_latest_results(league = league, season = season)
## That should always write a 'latest.csv' file
## Value of 'results' will be NULL if there are no new results

if (!is.null(results)) {
    if (FALSE) { # (match_in_play(league, season)) {
        notify_me(league, ":large_red_square: Still in play: alt3 table not updated")
    } else {
    fetch_league_table(league, season, source = "BBC")
    process_league_table_new(league, season)
    make_sched_strength_pages(league, season)
    temp <- alt3(league, damping = 0, consistency = 0.5, season = season)
    webify3(league, season)
    }
}

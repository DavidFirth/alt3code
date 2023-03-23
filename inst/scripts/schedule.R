#!/usr/bin/env Rscript
## Usage: e.g.,
##   Rscript schedule.R spain-la-liga-primera
## or (if not for today)
##   Rscript schedule.R spain-la-liga-primera 2021-05-02

args <-  commandArgs(trailingOnly = TRUE)
league <- args[1]
message(league)
the_date <- if (length(args) == 1) Sys.Date() else args[2]

season <- "2022"

library(devtools)
load_all(Sys.getenv("ALT3_CODE"))
setwd(paste0(Sys.getenv("ALT3_HOME"), "/docs/assets/leagues"))

notify_me <- slack

schedule <- read.csv(paste(league, "/", season, "/latest.csv", sep = ""))
kickoffs_today <- schedule$UTCtime[schedule$date == the_date]

if (length(kickoffs_today) > 0) {
    kickoffs_today <- unique(kickoffs_today)
    kickoffs_today <- paste(the_date, kickoffs_today)
    kickoffs_today <- strptime(kickoffs_today, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")
    kickoffs_today <- as.POSIXct(kickoffs_today, tz = Sys.timezone())
    updates_today <- kickoffs_today + (119 * 60) ## allow 2 hrs less 1 minute
    pushes_today <-  kickoffs_today + (121 * 60)
    updates_today <- substr(updates_today, 12, 16)  # hh:mm only
    pushes_today <- substr(pushes_today, 12, 16)
    for (pp in pushes_today){
        cat(pp, "\n",
            file = paste0(Sys.getenv("ALT3_SCRIPTS"), "/push-times-today.txt"),
            append = TRUE, sep = "")
    }
    notify_me(head = league,
              body = paste(":large_purple_square: Update scheduled today at",
                           paste(updates_today, collapse = " ")))
}

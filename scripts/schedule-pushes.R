#!/usr/bin/env Rscript

library(devtools)
load_all(Sys.getenv("ALT3_CODE"))
ALT3_SCRIPTS <- Sys.getenv("ALT3_SCRIPTS")

notify_me <- slack

schedule <- character(0)
if (file.exists(paste0(ALT3_SCRIPTS, "/push-times-today.txt")){
    schedule <-  unique(scan(paste0(ALT3_SCRIPTS, "/push-times-today.txt"),
                             what = character(0), sep = "\n"))
}

schedule <- sort(schedule)

if (length(schedule) > 0) {
    for (tt in schedule){
        system(paste(
            "echo '/usr/bin/Rscript /push.R' | at",
            tt, "today", sep = " "))
    }
    notify_me(head = "Pushes scheduled",
              body = paste(":large_purple_square: Today at",
                           paste(schedule, collapse = " ")))
} else {
    notify_me(head = "Pushes scheduled",
              body = ":large_purple_square: No matches today"
              )
}

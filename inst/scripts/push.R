#!/usr/bin/env Rscript

library(devtools)
load_all(Sys.getenv("ALT3_CODE"))

notify_me <- slack

pushcommand <- paste0("cd ", Sys.getenv("ALT3_HOME"), "; git push origin master")
test <- try(system(pushcommand))
print(test)

if (test != 0) {
    notify_me(head = "Push to GitHub",
              body = ":large_red_square: Failed"
              )
} else {
    notify_me(head = "Push to GitHub",
              body = ":large_green_square: Succeeded"
              )
}

cat("\n")


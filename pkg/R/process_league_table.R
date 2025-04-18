##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##' @title
##' @param league
##' @param html
##' @param csv
##' @return
##' @author David Firth
process_league_table_new <- function(league, season,
                                 html = "leagueTable.html",
                                 csv = "leagueTable.csv") {
    dirname <- paste0(league, "/", season)
    teams <- read.csv(paste0(dirname, "/", "namesOfTeams.csv"))
    row.names(teams) <- teams$BBC_name
    nteams <- nrow(teams)
    system(paste0(Sys.getenv("ALT3_SCRIPTS"), "/html-to-text.sh ", dirname, "/", html, " ",
                  dirname, "/leagueTable.txt"))
    mm <- scan(paste0(dirname, "/leagueTable.txt"),
             what = character(), sep = "\n",
             blank.lines.skip = FALSE)

    mm <- c(mm, "")  ## this is a fudge to cover inadequacy of my shell script
    ncols <- 10
    mm <- matrix(mm, (1 + nteams), ncols, byrow = TRUE)
    mm <- mm[-1,]
    mm <- cbind(as.character(1:nteams), mm)
    cn <- c("Position", "Team", "P", "W", "D", "L", "F", "A", "GD", "Pts", "Form")
    colnames(mm) <- cn
    mm[, "Team"] <- sub("[0-9]*", "", mm[, "Team"])
    myframe <- data.frame(team = mm[, "Team"],
                          teamId = "",
                          playedGames = as.numeric(mm[, "P"]),
                          goalDifference = as.numeric(mm[, "GD"]),
                          points = as.numeric(mm[, "Pts"]),
                          rank = as.numeric(mm[, 1]))
    myframe$teamId <- teams[as.character(myframe$team), "fdo_id"]
    myframe$team <- teams[as.character(myframe$team), "fdo_name"]
    write.csv(myframe,
              file = paste0(dirname, "/", csv),
              row.names = FALSE)
}

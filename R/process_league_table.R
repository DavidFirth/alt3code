##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##' @title
##' @param league
##' @param json
##' @param csv
##' @return
##' @author David Firth
process_league_table <- function(league, season = 2020,
                                 json = "leagueTable.json",
                                 csv = "leagueTable.csv") {
    require(jsonlite)
    league_df <- fromJSON(paste0(league, "/", season, "/", json)) $ standings$ table
    league_df <- league_df[[1]]
    team <- as.character(league_df $ team $ name)
    teamId <- as.character(league_df $ team $ id)
    league_df <- league_df[, c("playedGames", "goalDifference",
                               "points", "position")]
    league_df <- cbind(team, teamId, league_df)
    names(league_df) <- gsub("position", "rank", names(league_df))
    write.csv(league_df, paste(league, "/", season, "/", csv, sep = ""),
              row.names = FALSE)
}

process_league_table_new <- function(league, season = 2020,
                                 html = "leagueTable.html",
                                 csv = "leagueTable.csv") {
    dirname <- paste0(league, "/", season)
    teams <- read.csv(paste0(dirname, "/", "namesOfTeams.csv"))
    row.names(teams) <- teams$BBC_name
    nteams <- nrow(teams)
    system(paste0("./html-to-text.sh ", dirname, "/", html, " ",
                  dirname, "/leagueTable.txt"))
    mm <- matrix(scan(paste0(dirname, "/leagueTable.txt"),
                      what = character(), sep = "\n",
                      blank.lines.skip = FALSE),
                 (1 + nteams), 10, byrow = TRUE)
    colnames(mm) <- mm[1,]
    mm <- mm[-1,]
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

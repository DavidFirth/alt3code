##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##' @title
##' @param league
##' @param json
##' @param csv
##' @return
##' @author David Firth
process_league_table <- function(league, season = 2017,
                                 json = "leagueTable.json",
                                 csv = "leagueTable.csv") {
    require(jsonlite)
    league_df <- fromJSON(paste0(league, "/", season, "/", json)) $ standing
    league_df <- league_df[, c("team", "teamId", "playedGames", "goalDifference",
                               "points", "rank")]
    league_df $ teamId <- as.character(league_df $ teamId)
    write.csv(league_df, paste(league, "/", season, "/", csv, sep = ""), row.names = FALSE)
}

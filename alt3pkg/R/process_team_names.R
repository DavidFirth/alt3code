##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##' @title
##' @param league
##' @param season
##' @param json
##' @param csv
##' @return
##' @author David Firth
process_team_names <- function(league, season,
                                   json = "teams.json",
                                   csv = "teams.csv")
{
    teams <- jsonlite::fromJSON(paste(league, "/", season, "/", json,
                                    sep = "")) $ teams
    spreadsheet <- data.frame(fdo_id = teams$id,
                              fdo_name = teams$name,
                              short_name = teams$shortName,
                              abbrev = teams$tla,
                              sky_name = NA,
                              BBC_name = NA,
                              deduction = 0)
    write.csv(spreadsheet, paste(league, "/", season, "/", csv, sep = ""),
              row.names = FALSE)
    invisible(spreadsheet)
}

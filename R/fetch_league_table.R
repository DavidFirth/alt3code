##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##' @title
##' @param league
##' @param season
##' @param matchday
##' @param api_key_file
##' @return
##' @author David Firth
fetch_league_table <- function(league, season = 2017, matchday = NULL,
                               api_key_file = NULL){
##  Run this in the "leagues" directory.
    if (!(league %in% list.files())) stop("league folder not found")
    leagues <- read.csv(paste("leagues-", season, ".csv", sep = ""), row.names = 1)
    n_matchdays <- leagues[league, "n_matchdays"]
    league_id <- leagues[league, "fdo_id"]
    if (!is.null(matchday)) {
        matchday_query <- paste("?matchday=", matchday, sep = "")
        matchday_filename <- paste("leagueTable-", "matchday", matchday, ".json", sep = "")
        } else matchday_query <- paste0("?matchday=", n_matchdays)
    if (!is.null(api_key_file)) {
        api_key <- readLines(api_key_file)
        key_header <- paste("-H 'X-Auth-Token: ", api_key[1], "' ", sep = "")
    } else key_header <- ""
    url_header <- paste("-X GET http://api.football-data.org/v1/competitions/",
                        league_id, "/leagueTable", matchday_query, sep = "")
    curl_args <- paste("-H 'X-Response-Control: minified' ", key_header, url_header,
                       sep = "")
    outfile <- paste(league, "/", season, "/",
                     if (is.null(matchday)) "leagueTable.json" else matchday_filename,
                     sep = "")
    r <- system2("curl", curl_args, stdout = outfile)
    if (r == 0) cat("The data are in: ", outfile, "\n")
    else cat("There was an error!\n")
    invisible(r)}

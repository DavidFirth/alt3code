##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##' @title
##' @param league
##' @param season
##' @param matchday
##' @return
##' @author David Firth
fetch_results <- function(league, season, matchday = NULL) {
    if (!(league %in% list.files())) stop("league folder not found")
    leagues <- read.csv(paste("leagues-", season, ".csv", sep = ""), row.names = 1)
    league_id <- leagues[league, "fdo_id"]
    if (!is.null(matchday)) {
        matchday_query <- paste("?matchday=", matchday, sep = "")
        matchday_filename <- paste("matchday", matchday, ".json", sep = "")
    } else matchday_query <- ""

    api_key <- Sys.getenv("FDO_APIKEY")
    key_header <- paste("-H 'X-Auth-Token: ", api_key, "' ", sep = "")

    url_header <- paste("-X GET http://api.football-data.org/v2/competitions/",
                        league_id, "/matches", matchday_query, sep = "")
    curl_args <- paste("-H 'X-Response-Control: minified' ", key_header, url_header,
                       sep = "")
    outfile <- paste(league, "/", season, "/",
                     if (is.null(matchday)) "latest.json" else matchday_filename,
                     sep = "")
    r <- system2("curl", curl_args, stdout = outfile)
    if (r == 0) cat("The data are in: ", outfile, "\n")
    else cat("There was an error!\n")
    invisible(r)
}



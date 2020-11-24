##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##' @title
##' @param league
##' @param season
##' @param api_key_file
##' @return
##' @author David Firth
fetch_teams <- function(league, season = 2020, api_key_file = NULL) {
    if (!(league %in% list.files())) stop("league folder not found")
    leagues <- read.csv(paste("leagues-", season, ".csv", sep = ""), row.names = 1)
    league_id <- leagues[league, "fdo_id"]
    if (!is.null(api_key_file)) {
        api_key <- readLines(api_key_file)
        key_header <- paste("-H 'X-Auth-Token: ", api_key[1], "' ", sep = "")
    } else key_header <- ""
    url_header <- paste("-X GET http://api.football-data.org/v2/competitions/",
                        league_id, "/teams", sep = "")
    curl_args <- paste("-H 'X-Response-Control: minified' ", key_header, url_header,
                       sep = "")
    outfile <- paste(league, "/", season, "/",
                     "teams.json",
                     sep = "")
    r <- system2("curl", curl_args, stdout = outfile)
    if (r == 0) cat("The data are in: ", outfile, "\n")
    else cat("There was an error!\n")
    invisible(r)
}

##  eg fetch_teams("england-championship", api_key_file = "/data/david/work/football/api-key.txt")

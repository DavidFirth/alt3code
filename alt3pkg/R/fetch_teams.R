##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##' @title
##' @param league
##' @param season
##' @return
##' @author David Firth
fetch_teams <- function(league, season) {
    if (!(league %in% list.files())) stop("league folder not found")
    leagues <- read.csv(paste("leagues-", season, ".csv", sep = ""), row.names = 1)
    league_id <- leagues[league, "fdo_id"]

    api_key <- Sys.getenv("FDO_APIKEY")
    key_header <- paste("-H 'X-Auth-Token: ", api_key, "' ", sep = "")

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

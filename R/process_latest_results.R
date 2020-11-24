##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##' @title
##' @param league
##' @param season
##' @param json
##' @param csv
##' @param keep
##' @return
##' @author David Firth
process_latest_results <- function(league, season = 2020,
                                   json = "latest.json",
                                   csv = "latest.csv",
                                   keep = TRUE) {
    require(jsonlite)
    match_results <- fromJSON(paste(league, "/", season, "/", json,
                                    sep = "")) $ matches
    scores <- match_results $ score $ fullTime
    finished <- (match_results $ status) == "FINISHED"
    matchday <- match_results $ matchday
    date <- substr(match_results$utcDate, 1, 10)
    homeTeamName <- match_results $ homeTeam $ name
    homeTeamId <- match_results $ homeTeam $ id
    awayTeamName <- match_results $ awayTeam $ name
    awayTeamId <- match_results $ awayTeam $ id
    FTHG <- scores $ homeTeam
    FTAG <- scores $ awayTeam
    spreadsheet <- data.frame(matchday = matchday,
                              homeTeamName = homeTeamName,
                              homeTeamId = homeTeamId,
                              awayTeamName = awayTeamName,
                              awayTeamId = awayTeamId,
                              FTHG = FTHG,
                              FTAG = FTAG)

    spreadsheet <- within(spreadsheet, {
        FTR <- "Z"
        FTR[(FTHG == FTAG) & finished] <- "D"
        FTR[(FTHG > FTAG) & finished] <- "H"
        FTR[(FTHG < FTAG) & finished] <- "A"
        FTR[FTR == "Z"] <- NA
        FTR <- factor(FTR)
    })
    spreadsheet $ date <-  date
    write.csv(spreadsheet, paste(league, "/", season, "/", csv, sep = ""),
              row.names = FALSE)
    file_path <- NULL
    if (keep) {
        n_results <- sum(!is.na(spreadsheet $ FTR))
        file_name <- paste(sprintf("results%03d", n_results), ".csv", sep = "")
        results_dir <- paste(league, "/", season, sep = "")
        if (!(file_name %in% list.files(results_dir))) {
            file_path <- paste(results_dir, "/", file_name, sep = "")
            write.csv(spreadsheet, file_path, row.names = FALSE)
        }
    }
    return(file_path)
}

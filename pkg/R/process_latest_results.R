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
process_latest_results <- function(league, season,
                                   json = "latest.json",
                                   csv = "latest.csv",
                                   keep = TRUE) {
    match_results <- jsonlite::fromJSON(paste(league, "/", season, "/", json,
                                              sep = "")) $ matches
    scores <- match_results $ score $ fullTime
    finished <- (match_results $ status) %in% c("FINISHED", "AWARDED")
    postponed <- (match_results $ status) %in% c("POSTPONED", "CANCELLED")
    matchday <- match_results $ matchday
    date <- substr(match_results$utcDate, 1, 10)
    UTCtime <- substr(match_results$utcDate, 12, 19)
    homeTeamName <- match_results $ homeTeam $ name
    homeTeamId <- match_results $ homeTeam $ id
    awayTeamName <- match_results $ awayTeam $ name
    awayTeamId <- match_results $ awayTeam $ id
    FTHG <- scores $ home
    FTAG <- scores $ away
    is.na(FTHG[postponed]) <- TRUE
    is.na(FTAG[postponed]) <- TRUE
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
    spreadsheet $ UTCtime <- UTCtime
    spreadsheet $ status <- match_results $ status
## Next few lines move unscheduled postponements to the bottom of the sheet
    n_rows <- nrow(spreadsheet)
    match_result_rows <- which(!is.na(spreadsheet $ FTR))
    last_match_result_row <- match_result_rows[length(match_result_rows)]
    unscheduled_postponements <- postponed[1:last_match_result_row]
    if (any(unscheduled_postponements)) {
        unscheduled_postponements <- which(unscheduled_postponements)
        results_and_scheduled <- (1:n_rows)[-unscheduled_postponements]
        spreadsheet <- spreadsheet[c(results_and_scheduled, unscheduled_postponements), ]
        row.names(spreadsheet) <- 1:n_rows
    }
## Write the spreadsheet to disk
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

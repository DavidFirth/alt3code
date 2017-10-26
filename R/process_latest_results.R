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
process_latest_results <- function(league, season = 2017,
                                   json = "latest.json",
                                   csv = "latest.csv",
                                   keep = TRUE) {
    require(jsonlite)
    match_results <- fromJSON(paste(league, "/", season, "/", json, sep = "")) $ fixtures
    scores <- match_results $ result[, 1:2]
    finished <- (match_results $ status) == "FINISHED"
    match_results $ result <- NULL
    match_results$date <- match_results$status <- match_results$id <-
        match_results$competitionId <- match_results $ odds <- NULL
    match_results $ FTHG <- scores[, 1]
    match_results $ FTAG <- scores[, 2]
    match_results <- within(match_results, {
        FTR <- "Z"
        FTR[(FTHG == FTAG) & finished] <- "D"
        FTR[(FTHG > FTAG) & finished] <- "H"
        FTR[(FTHG < FTAG) & finished] <- "A"
        FTR[FTR == "Z"] <- NA
        FTR <- factor(FTR)
    })
    write.csv(match_results, paste(league, "/", season, "/", csv, sep = ""),
              row.names = FALSE)
    if (keep) {
        n_results <- sum(!is.na(match_results $ FTR))
        file_name <- paste(sprintf("results%03d", n_results), ".csv", sep = "")
        results_dir <- paste(league, "/", season, sep = "")
        if (!(file_name %in% list.files(results_dir))) {
            write.csv(match_results, paste(results_dir, "/",
                                           file_name, sep = ""),
                      row.names = FALSE)
        }
    }
    invisible(match_results)
}

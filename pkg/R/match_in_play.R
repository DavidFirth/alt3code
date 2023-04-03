match_in_play <- function(league, season, results = "latest.csv") {
    teams_and_results <- alt3_data(league, season, results)
    results <- teams_and_results $ results
    in_play <- results$status %in% c("IN_PLAY", "PAUSED")
    return(any(in_play))
}

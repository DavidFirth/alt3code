alt3_model <- function(teams_and_results, damping, consistency){

    prior_needed <- (damping > 0) || (consistency > 0)
    if (prior_needed) {
        prior_data <- make_prior_data(teams_and_results $ teams $ shortName,
                                      damping, consistency)
    }

    results <- teams_and_results $ results
    teamNames <- teams_and_results $ teams $ shortName
    names(teamNames) <- row.names(teams_and_results $ teams)
    nTeams <- length(teamNames)

    results $ homeTeam <- teamNames[as.character(results $ homeTeamId)]
    results $ awayTeam <- teamNames[as.character(results $ awayTeamId)]
    results <- results[, c("matchday", "homeTeam", "awayTeam", "FTHG", "FTAG", "FTR")]
    results $ unplayed <- is.na(results $ FTR)
    results <- cbind(sprintf("%03d", as.numeric(row.names(results))),
                             results)
    names(results)[1] <- "match"
    results$FTR <- factor(results$FTR, levels = c("A", "D", "H"))
    modelframe <- gnm::expandCategorical(results, "FTR", idvar = "match")
    modelframe $ FTHG <- modelframe $ FTAG <- NULL
    is.na(modelframe[modelframe $ unplayed, "count"]) <- TRUE
    played <- !(modelframe $ unplayed)
    modelframe $ unplayed <- NULL

    modelframe$draw <- as.numeric(modelframe$FTR == "D")

    if (prior_needed) modelframe <- rbind(prior_data, modelframe)
    modelframe$match <- factor(modelframe$match)

    X <- matrix(0, nrow(modelframe), 2 * nTeams)
    colnames(X) <- paste0(teamNames, c(rep("_home", nTeams), rep("_away", nTeams)))
    for (team in teamNames) {
        X[modelframe$homeTeam == team & modelframe$FTR == "H", paste0(team, "_home")] <- 1
        X[modelframe$homeTeam == team & modelframe$FTR == "D", paste0(team, "_home")] <- 1/3
        X[modelframe$awayTeam == team & modelframe$FTR == "A", paste0(team, "_away")] <- 1
        X[modelframe$awayTeam == team & modelframe$FTR == "D", paste0(team, "_away")] <- 1/3
    }

    modelframe$s <- X
    model <- gnm::gnm(count ~ -1 + s + draw,
                      eliminate = match,
                      family = quasipoisson,
                      data = modelframe,
                      start = rep(0, 2 * nTeams + 1))
    names(model$coefficients) <- c(colnames(X), "draw")
    return(model)
}

make_prior_data <- function(league, season, damping_weight, consistency_weight){
    teamNames <- read.csv(paste0(league, "/", season, "/", "namesOfTeams.csv"),
                          as.is = TRUE)
    nTeams <- nrow(teamNames)
    teamNames <- teamNames $ abbrev
    teamNames <- rep(teamNames, rep(2, nTeams))
    matchNames <- paste0(teamNames, rep(c("_home", "_away"), 2 * rep(nTeams, 2)))
    result1 <- data.frame(match = paste0("prior.", matchNames),
                         matchday = 0,
                         homeTeam = c(teamNames, rep("average", length(teamNames))),
                         awayTeam = c(rep("average", length(teamNames)), teamNames),
                         FTR = rep(c("A", "H"), nTeams),
                         count = damping_weight * rep(c(1, 1), nTeams),
                         draw = 0, home = 0)
    matchNames <- teamNames
cat("blah")
    result2 <- data.frame(match = paste0("prior.", matchNames),
                         matchday = 0,
                         homeTeam = matchNames,
                         awayTeam = matchNames,
                         FTR = rep(c("A", "H"), nTeams),
                         count = consistency_weight * rep(c(1, 1), nTeams),
                         draw = 0, home = 0)
    return(rbind(result1, result2))
}

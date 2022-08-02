make_prior_data <- function(teamNames, damping, consistency){
    nTeams <- length(teamNames)
    teamNames <- rep(teamNames, rep(2, nTeams))
    matchNames <- paste0(teamNames, rep(c("_home", "_away"), 2 * rep(nTeams, 2)))
    damping_prior <- data.frame(match = paste0("damping.", matchNames),
                         matchday = 0,
                         homeTeam = c(teamNames, rep("average", length(teamNames))),
                         awayTeam = c(rep("average", length(teamNames)), teamNames),
                         FTR = rep(c("A", "H"), nTeams),
                         count = damping * rep(c(1, 1), nTeams),
                         draw = 0)
    matchNames <- teamNames
    consistency_prior <- data.frame(match = paste0("consistency.", matchNames),
                         matchday = 0,
                         homeTeam = matchNames,
                         awayTeam = matchNames,
                         FTR = rep(c("A", "H"), nTeams),
                         count = consistency * rep(c(1, 1), nTeams),
                         draw = 0)
    return(rbind(damping_prior, consistency_prior))
}

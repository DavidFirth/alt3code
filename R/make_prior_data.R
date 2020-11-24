make_prior_data <- function(league, season = 2020, prior_weight = 1){
    teamNames <- read.csv(paste0(league, "/", season, "/", "namesOfTeams.csv"),
                          as.is = TRUE)
    nTeams <- nrow(teamNames)
    teamNames <- teamNames $ abbrev
    teamNames <- rep(teamNames, rep(2, nTeams))
    result <- data.frame(match = paste0("prior.", teamNames),
                         matchday = 0,
                         homeTeam = teamNames,
                         awayTeam = "average",
                         FTR = rep(c("A", "H"), nTeams),
                         count = prior_weight * rep(c(0.5, 0.5), nTeams),
                         draw = 0, home = 0)
    return(result)
}



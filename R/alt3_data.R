alt3_data <- function(league, season, results){
    leagues_dir <- Sys.getenv("ALT3_LEAGUES_DIR")
    leagues <- read.csv(paste0(leagues_dir, "leagues-", season, ".csv"), row.names = 1)
    dirname <- paste0(leagues_dir, league, "/", season, "/")
    results <- read.csv(paste0(dirname, results), as.is = TRUE)
    teamNames <- read.csv(paste0(dirname, "namesOfTeams.csv"), as.is = TRUE)
    nTeams <- nrow(teamNames)
    nWeeks <- 2 * (nTeams - 1)
    row.names(teamNames) <- teamNames$"fdo_id"
    deduction <- teamNames $ deduction
    names(deduction) <- teamNames $ abbrev
    teamId <- teamNames $ "fdo_id"
    longName <- teamNames $ short_name
    shortName <- teamNames $ abbrev
    names(shortName) <- names(longName) <- teamId
    teams <- data.frame(shortName, longName, teamId, deduction)
    return(list(teams = teams, results = results))
}

alt3 <- function(league, season, results = "latest.csv",
                 damping, consistency,
                 outfile = NULL,
                 check_table = TRUE) {
    dirname <- paste0(league, "/", season, "/")
    teams_and_results <- alt3_data(league, season, results)
    teams <- teams_and_results $ teams
    results <- teams_and_results $ results
    not_in_play <- !(results$status %in% c("IN_PLAY", "PAUSED"))
    nTeams <- nrow(teams)
    nWeeks <- 2 * (nTeams - 1)
    teamNames <- teams $ shortName
    longNames <- teams $ longName
    names(teamNames) <- names(longNames) <- row.names(teams)
    deduction <- teams $ deduction

    prior_data <- make_prior_data(teams_and_results $ teams $ shortName,
                                  damping, consistency)

    results $ homeTeam <- teamNames[as.character(results $ homeTeamId)]
    results $ awayTeam <- teamNames[as.character(results $ awayTeamId)]
    results <- results[, c("matchday", "homeTeam", "awayTeam", "FTHG", "FTAG", "FTR")]
    results $ unplayed <- is.na(results $ FTR)
    results <- cbind(sprintf("%03d", as.numeric(row.names(results))),
                             results)
    names(results)[1] <- "match"
    results$FTR <- factor(results$FTR, levels = c("A", "D", "H"))

    fitted_params <- coef(alt3_model(teams_and_results,
                                     damping, consistency))
    if (is.na(fitted_params["draw"])) fitted_params["draw"] <- 0

    log_strengths <- fitted_params[1:(2 * nTeams)]
    log_delta <- fitted_params["draw"]

    ## The following is just to center the log-strengths, when that has not been done
    ## already by the prior penalty.  FAULTY!  Check it!
    ##
    if (damping == 0) {
        mean_strength <- logistic_mean(log_strengths)
        log_strengths <- log_strengths - mean_strength
        log_delta <- log_delta - 1/3 * mean_strength
    }

    strengths <- matrix(exp(log_strengths), nTeams, 2)
    rownames(strengths) <- teamNames
    colnames(strengths) <- c("home", "away")

    delta <- exp(log_delta)

    ## Next part assumes every team has played at least one match at home and away
    ##
    ## This could be relaxed, though?
    ##
    sched_list <- sched.s(league, season, results, strengths, delta)

    ePld <- unlist(lapply(sched_list,
                   function(ss.i) sum((ss.i $ ePts) * (ss.i $ played)) / mean(ss.i $ ePts)
                   ))
    Pld <- unlist(lapply(sched_list,
                  function(ss.i) sum(na.omit(ss.i $ played))
                  ))
    Pts <- unlist(lapply(sched_list,
                  function(ss.i) sum(na.omit(ss.i $ Pts))
                  ))

    GD <- unlist(lapply(sched_list,
                  function(ss.i) sum(na.omit(ss.i $ GD))
                  ))

    GS <- unlist(lapply(sched_list,
                  function(ss.i) sum(na.omit(ss.i $ GS))
                  ))

    alt3_rate <- round(Pts/ePld - deduction/nWeeks, 4)

    home_Pts <- unlist(lapply(sched_list,
                              function(ss.i) sum(na.omit(ss.i[ss.i$HA == "H", ] $ Pts))
                              ))
    home_Pld <- unlist(lapply(sched_list,
                              function(ss.i) sum(na.omit(ss.i[ss.i$HA == "H", ] $ played))
                              ))
    home_ePld <- unlist(lapply(sched_list,
                               function(ss.i) {
        home_rows <- ss.i[ss.i$HA == "H", ]
        sum((home_rows $ ePts) * (home_rows $ played)) / mean(home_rows $ ePts)
    }
    ))
    #home_rate <- round(home_Pts / home_ePld - deduction / nWeeks, 4)
    home_rate <- home_Pts / home_Pld

    away_Pts <- unlist(lapply(sched_list,
                  function(ss.i) sum(na.omit(ss.i[ss.i$HA == "A", ] $ Pts))
                  ))
    away_Pld <- unlist(lapply(sched_list,
                  function(ss.i) sum(na.omit(ss.i[ss.i$HA == "A", ] $ played))
                  ))
    away_ePld <- unlist(lapply(sched_list,
                               function(ss.i) {
        away_rows <- ss.i[ss.i$HA == "A", ]
        sum((away_rows $ ePts) * (away_rows $ played)) / mean(away_rows $ ePts)
    }
    ))
    #away_rate <- round(away_Pts / away_ePld - deduction / nWeeks, 4)
    away_rate <- away_Pts / away_Pld

    rates <- data.frame(away_rate, home_rate, alt3_rate)

    alt3_rate <- round(alt3_rate, 2)
    ePld <- round(ePld, 1)
    Pts <- Pts - deduction

    standard_table <- read.csv(paste0(dirname, "leagueTable.csv"), as.is = TRUE)
    row.names(standard_table) <- teamNames[as.character(standard_table$teamId)]
    longNames <- longNames[as.character(standard_table$teamId)]
    standard_table $ long_names <- longNames
    standard_table <- standard_table[, c(7, 3:6)]
    names(standard_table) <- c("longnames", "Pld", "GD", "Pts", "rank")
    standard_table <- standard_table[names(alt3_rate), ]
    if (check_table) {
        if (any(Pts != standard_table$Pts)) {
            notify_me(league,
                      ":large_red_square: Points differ from official table: stopping")
            browser()
            stop("Points totals differ!")
        }
        if (any(GD != standard_table$GD)) {
            notify_me(league,
                      ":large_red_square: GD differs from official table: continuing")
                                        # stop("Goal differences differ!")
                                        #
                                        # previous line commented because it's unclear
                                        # what to do about incorrect BBC table
        }
    }

    if (all(ePld == Pld)) ordering <- order(standard_table$rank)
    else ordering <- order(alt3_rate, -(standard_table $ rank), decreasing = TRUE)
    alt3_rate <- alt3_rate[ordering]
    ePld <- ePld[ordering]
    standard_table <- standard_table[names(alt3_rate), ]
    res <- data.frame(ePld = ePld, Rate = alt3_rate)
    res <- cbind(1:nrow(res), res)
    names(res)[1] <- "rank+"
    result <- cbind(standard_table[row.names(res),], res)
    if (is.null(outfile)) outfile <- paste0(league, "/", season, "/alt3.csv")
    write.csv(result, file = outfile)

    plot_schedule_strengths(sched_list = sched_list, league = league,
                            season = season)

    svg(file = paste0("../../_includes/leagues/", league, "/", "rates.svg"))
    plot_rates(rates)
    dev.off()
    return(result)
}

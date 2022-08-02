alt3 <- function(league, season, results = "latest.csv", prior_weight = 0.01,
                 outfile = NULL, check_table = TRUE) {

    leagues <- read.csv(paste("leagues-", season, ".csv", sep = ""),
                        row.names = 1)
    dirname <- paste0(league, "/", season, "/")
    results <- read.csv(paste0(dirname, results), as.is = TRUE)
    not_in_play <- !(results$status %in% c("IN_PLAY", "PAUSED"))
    teamNames <- read.csv(paste0(league, "/", season, "/", "namesOfTeams.csv"),
                          as.is = TRUE)
    nTeams <- nrow(teamNames)
    nWeeks <- 2 * (nTeams - 1)
    row.names(teamNames) <- teamNames$"fdo_id"
    deduction <- teamNames $ deduction
    names(deduction) <- teamNames $ abbrev
    teamId <- teamNames $ "fdo_id"
    longNames <- teamNames $ short_name
    teamNames <- teamNames $ abbrev
    names(teamNames) <- names(longNames) <- teamId

    prior_data <- make_prior_data(league, season, prior_weight)

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
    modelframe$home <- ((modelframe$FTR == "H") - (modelframe$FTR == "A"))/2
    modelframe <- rbind(prior_data, modelframe)
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
                      data = modelframe)
    names(model$coefficients) <- c(colnames(X), "draw")
    coefs <- coef(model)
    delta <- exp(coefs["draw"])
    strengths <- matrix(exp(coefs[1:(2 * nTeams)]), nTeams, 2)
    rownames(strengths) <- teamNames
    colnames(strengths) <- c("home", "away")

    ## Next part assumes every team has played at least one match at home and away
    ##
    sched_list<- sched.s(league, season, results, strengths, delta)

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

    alt3_rate <- Pts/ePld - deduction/nWeeks

    home_Pts <- unlist(lapply(sched_list,
                              function(ss.i) sum(na.omit(ss.i[ss.i$HA == "H", ] $ Pts))
                              ))
    home_ePld <- unlist(lapply(sched_list,
                               function(ss.i) {
        home_rows <- ss.i[ss.i$HA == "H", ]
        sum((home_rows $ ePts) * (home_rows $ played)) / mean(home_rows $ ePts)
    }
    ))
    home_rate <- home_Pts / home_ePld - deduction / nWeeks

    away_Pts <- unlist(lapply(sched_list,
                  function(ss.i) sum(na.omit(ss.i[ss.i$HA == "A", ] $ Pts))
                  ))
    away_ePld <- unlist(lapply(sched_list,
                               function(ss.i) {
        away_rows <- ss.i[ss.i$HA == "A", ]
        sum((away_rows $ ePts) * (away_rows $ played)) / mean(away_rows $ ePts)
    }
    ))
    away_rate <- away_Pts / away_ePld - deduction / nWeeks

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
        if (all(not_in_play)){ # so current BBC table should agree with Pts and GD
            if (any(Pts != standard_table$Pts)) {
                notify_me(league,
                          ":large_red_square: Points differ from official table: stopping")
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
        } else { ## at least one match is still in play
            notify_me(league, ":large_red_square: Still in play: stopping")
            stop("At least one match is still in play")
        }
    }
    if (all(ePld == Pld)) ordering <- order(standard_table$rank)
    else ordering <- order(alt3_rate, standard_table $ Pts, standard_table $ GD,
                      decreasing = TRUE)
    alt3_rate <- alt3_rate[ordering]
    ePld <- ePld[ordering]
    standard_table <- standard_table[names(alt3_rate), ]
    res <- data.frame(ePld = ePld, Rate = alt3_rate)
    res <- cbind(1:nrow(res), res)
    names(res)[1] <- "rank+"
    result <- cbind(standard_table[row.names(res),], res)
    if (is.null(outfile)) outfile <- paste0(league, "/", season, "/alt3.csv")
    write.csv(result, file = outfile)

    require(ggplot2)

    plot_schedule_strengths(sched_list = sched_list, league = league,
                            season = season)

    svg(file = paste0("../../_includes/leagues/", league, "/", "rates.svg"))
    plot_rates(rates)
    dev.off()

    return(result)
}



sched.s.old <- function(league, season, results, strengths, apm, draw_par, home_par){
##  results should be only those matches played
    dirname <- paste(league, season, "schedule-strengths", sep = "/")
    ssi <- function(i){
        results <- results[results$homeTeam == i |
                           results$awayTeam == i, ]
        results$HA <- "H"
        results$HA[results$awayTeam == i] <- "A"
        opponent <- ifelse(results$HA == "H",
                           results$awayTeam,
                           results$homeTeam)
        s.i <- strengths[i]
        s.j <- ifelse(results$HA == "H",
                      strengths[results$awayTeam],
                      strengths[results$homeTeam])
        p.iwin <- ifelse(results$HA == "H",
                         s.i * exp(home_par / 2),
                         s.i * exp(-home_par / 2)
                         )
        p.jwin <- ifelse(results$HA == "H",
                         s.j * exp(-home_par / 2),
                         s.j * exp(home_par / 2)
                         )
        p.draw <- exp(draw_par) *  (s.i * s.j) ^ (1/3)
        denom <- p.iwin + p.jwin + p.draw
        p.iwin <- p.iwin / denom
        p.jwin <- p.jwin / denom
        p.draw <- p.draw / denom
        expected.points <- 3 * p.iwin + p.draw
        sched <-  (apm[i] - expected.points) / apm[i]
        res <- data.frame(HA = results$HA,
                   vs = opponent,
                   played = 1 - (results $ unplayed),
                   sched = sched,
                   cumulative = cumsum(sched)
                   )

        ## As a side-effect, write out a neat-ish .csv file for each team
        res.tidied <- res
        res.tidied $ played <- ifelse(res $ played == 1, "yes", "no")
        res.tidied $ sched <- sprintf("%.2f", res $ sched)
        res.tidied $ cumulative <- sprintf("%.2f", res $ cumulative)
        write.csv(res.tidied, file = paste0(dirname, "/", i, ".csv"))

        return(res)
    }
    result <- lapply(names(strengths), ssi)
    names(result) <- names(strengths)
    result
}

sched.s <- function(league, season, results, strengths, draw_par){
##  results should be only those matches played
    dirname <- paste(league, season, "schedule-strengths", sep = "/")
    ssi <- function(i){
        results <- results[results$homeTeam == i |
                           results$awayTeam == i, ]
        results$HA <- "H"
        results$HA[results$awayTeam == i] <- "A"
        results$Pts <- 3 * (results$FTR == results$HA) + 1 * (results$FTR == "D")
        results$GD <- (results$FTHG - results$FTAG) *
            ((results$HA == "H") - (results$HA == "A"))
        opponent <- ifelse(results$HA == "H",
                           results$awayTeam,
                           results$homeTeam)
        s.i <- ifelse(results$HA == "H",
                      strengths[i, "home"],
                      strengths[i, "away"])
        s.j <- ifelse(results$HA == "H",
                      strengths[results$awayTeam, "away"],
                      strengths[results$homeTeam, "home"])
        p.iwin <- s.i
        p.jwin <- s.j
        p.draw <- draw_par *  (s.i * s.j) ^ (1/3)
        denom <- p.iwin + p.jwin + p.draw
        p.iwin <- p.iwin / denom
        p.jwin <- p.jwin / denom
        p.draw <- p.draw / denom
        expected.points <- 3 * p.iwin + p.draw
        played <- 1 - results$unplayed
        apm <- mean(expected.points)
        sched <-  (apm - expected.points) / apm
        res <- data.frame(HA = results$HA,
                   vs = opponent,
                   played = 1 - (results $ unplayed),
                   Pts = results $ Pts,
                   GD = results $ GD,
                   ePts = expected.points,
                   sched = sched,
                   cumulative = cumsum(sched)
                   )

        ## As a side-effect, write out a neat-ish .csv file for each team
        res.tidied <- res
        res.tidied $ played <- ifelse(res $ played == 1, "yes", "no")
        res.tidied $ ePts <- sprintf("%.2f", res $ ePts)
        res.tidied $ sched <- sprintf("%.2f", res $ sched)
        res.tidied $ cumulative <- sprintf("%.2f", res $ cumulative)
        write.csv(res.tidied, file = paste0(dirname, "/", i, ".csv"))

        return(res)
    }
    result <- lapply(rownames(strengths), ssi)
    names(result) <- rownames(strengths)
    result
}

plot_schedule_strengths <- function(league, season, sched_list) {
    require(ggplot2)
    teams <- read.csv(paste(league, season, "namesOfTeams.csv", sep = "/"))
    nteams <- nrow(teams)
    nweeks <- 2 * (nteams - 1)
    plot_sched <- function(schedule, team, matches_played) {
        team_fullname <- teams$short_name[teams$abbrev == team]
        dirname <- paste("../../_includes/leagues",
                              league, "schedule-strengths/", sep = "/")
        svg(file = paste0(dirname, team, ".svg"), width = 4, height = 10, bg = "transparent")
        scalemax <- ceiling(max(abs(schedule $ cumulative)))
        n_played <- sum(schedule $ played)
        plot_the_now_line <- (n_played < nweeks) &&
            (cumsum(schedule $ played)[1:n_played] == 1:n_played)
        the_plot <- ggplot(schedule, aes(x = cumulative, y = 1:nweeks)) +
            geom_point(size = 0, stroke = 0) +
            geom_text(aes(label = vs, color = HA),
                      hjust = 0.5, vjust = 0.5,
                      show.legend = FALSE) +
            annotate("text", label = "Home matches in blue",
                     x = -scalemax + 0.05, y = -1.9, size = 3, fontface = "italic",
                     colour = "#000088", hjust = 0) +
            annotate("text", label = "Away matches in red",
                     x = -scalemax + 0.05, y = -1.2, size = 3, fontface = "italic",
                     colour = "#880000", hjust = 0) +
                                        # scale_color_hue(l = 40, c = 35) +
            ## not sure why scale_color_hue was used
            ## -- it seems to clash with scale_color_manual below, in any case
            ## so I am commenting it out now (Dec 2019)
            geom_label(aes(x = 0, y = -0.1, label = "Start of season"),
                       fill = "white") +
            geom_label(aes(x = 0, y = nweeks + 1.1, label = "End of season"),
                       fill = "white") +
            geom_label(aes(x = 0, y = (nteams - 0.5), label = "Halfway"),
                       color = "grey", fill = "white") +
            geom_hline(yintercept = -0.1, linetype = "solid",
                color = "black", size = 1) +
            geom_hline(yintercept = nweeks + 1.1, linetype = "solid",
                color = "black", size = 1) +
            annotate("text", label = "dot",
                     x = 0, y = nweeks + 1.4, size = 1,
                     colour = "#880000") +
            geom_hline(yintercept = nteams - 0.5, linetype = "dashed",
                color = "grey", size = 1)
        if (plot_the_now_line) {
            the_plot <- the_plot +
                geom_label(aes(x = 0, y = 0.5 + n_played, label = "Now"),
                           color = "darkgrey", fill = "white") +
                geom_hline(yintercept = 0.5 + n_played, linetype = "dashed",
                           color = "green", size = 1)
        }
        the_plot <- the_plot +
            geom_vline(xintercept = 0, linetype = "dashed",
                       color = "grey", size = 1) +
            scale_y_reverse(expand = c(0, 0.7)) +
            theme(axis.title.y = element_blank(),
                  axis.text.y = element_blank(),
                  axis.ticks.y = element_blank(),
                  axis.title.x = element_text(color = "darkgrey", size = 10)) +
            scale_x_continuous(position = "top",
                               limits = c(-scalemax, scalemax)) +
            labs(title = team_fullname,
                 subtitle = "Schedule strength (cumulative)",
                 x = " <<<< easier <<<<            >>>> harder >>>>") +
            theme(plot.title = element_text(face="bold", size = 16),
                  plot.margin = margin(0, 0, 0, 0, "cm"))
        the_plot $ layers = rev(the_plot $ layers)
        the_plot <- the_plot +
            scale_color_manual(values=c("#880000", "#000088"))
        the_plot <- the_plot + theme(
                                   panel.grid.major.y = element_blank(),
                                   panel.grid.minor.y = element_blank()
                               )
        print(the_plot)
        dev.off()
    }

    for (team in names(sched_list)) {
        sched <- sched_list[[team]]
        plot_sched(sched, team)
    }
}


make_sched_strength_pages <- function(league, season) {
    dirname <- paste(league, season, sep = "/")
    teamNames <- read.csv(paste0(dirname, "/", "namesOfTeams.csv"),
                          as.is = TRUE)
    shortname <- teamNames $ short_name
    abbrev <- teamNames $ abbrev
    tx  <- readLines("schedule_strength_template.md")
    tx <- gsub(pattern = "thisleague", replace = league, x = tx)
    for (i in seq(along = abbrev)) {
        include.text <- paste0("{% include leagues/", league,
                               "/schedule-strengths/", abbrev[i], ".svg %}")
        tx2  <- gsub(pattern = "include-svg-file-here",
                     replace = include.text, x = tx)
        tx3  <- gsub(pattern = "shortname", replace = shortname[i], x = tx2)
        tx4  <- gsub(pattern = "abbrev", replace = abbrev[i], x = tx3)
        tx5 <- gsub(pattern = "theseason", replace = season, x = tx4)
        writeLines(tx5, con = paste0(dirname, "/schedule-strengths/",
                                     abbrev[i], ".md"))
    }
}


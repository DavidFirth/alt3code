alt3 <- function(league, season = 2017, results = "latest.csv",
                            draw_par = NULL,
                            home_par = NULL,
                            prior_weight = NULL,
                            outfile = NULL) {

    leagues <- read.csv(paste("leagues-", season, ".csv", sep = ""),
                        row.names = 1)

    ##  Need to avoid the double-assign here by fixing gnm!
    if (is.null(draw_par)) draw_par <- leagues[league, "draw_par"]
    if (is.null(home_par)) home_par <- leagues[league, "home_par"]
    if (is.null(prior_weight)) prior_weight <- leagues[league, "prior_weight"]
    ## Next three lines needed only because of the bug in gnm
    ## -- but is the prior_weight assignment actually needed?
    draw_par <<- draw_par
    home_par <<- home_par
    prior_weight <<- prior_weight

    prior_data <- make_prior_data(league, season, prior_weight)

    dirname <- paste0(league, "/", season, "/")
    results <- read.csv(paste0(dirname, results), as.is = TRUE)

    teamNames <- read.csv(paste0(league, "/", season, "/", "namesOfTeams.csv"),
                          as.is = TRUE)
    nTeams <- nrow(teamNames)
    row.names(teamNames) <- teamNames$"fdo_id"
    teamId <- teamNames $ "fdo_id"
    longNames <- teamNames $ short_name
    teamNames <- teamNames $ abbrev
    names(teamNames) <- names(longNames) <- teamId

    results $ homeTeam <- teamNames[as.character(results $ homeTeamId)]
    results $ awayTeam <- teamNames[as.character(results $ awayTeamId)]

    results <- results[, c("matchday", "homeTeam", "awayTeam", "FTR")]
    results $ unplayed <- is.na(results $ FTR)
    results <- cbind(sprintf("%03d", as.numeric(row.names(results))),
                             results)
    names(results)[1] <- "match"

    modelframe <- gnm::expandCategorical(results, "FTR", idvar = "match")
    is.na(modelframe[modelframe $ unplayed, "count"]) <- TRUE
    played <- !(modelframe $ unplayed)
    modelframe $ unplayed <- NULL

    FTR <- modelframe $ FTR
    homeTeam <- modelframe $ homeTeam
    awayTeam <- modelframe $ awayTeam

    modelframe$draw <- as.numeric(FTR == "D")
    modelframe$home <- ((FTR == "H") - (FTR == "A"))/2

#    modelframe <- rbind(prior_data, modelframe)

    FTR <- modelframe $ FTR
    homeTeam <- modelframe $ homeTeam
    awayTeam <- modelframe $ awayTeam

    X <- matrix(0, nrow(modelframe), nTeams)
    colnames(X) <- teamNames

    for (team in teamNames) {
        X[homeTeam == team & FTR == "H", team] <- 1
        X[homeTeam == team & FTR == "D", team] <- 1/3
        X[awayTeam == team & FTR == "A", team] <- 1
        X[awayTeam == team & FTR == "D", team] <- 1/3
    }

    modelframe$s <- X
    model <- gnm::gnm(count ~ -1 + s,
                      offset = draw_par * draw + home_par * home,
                      eliminate = match,
                      family = poisson,
                      data = modelframe)
    names(model$coefficients) <- teamNames
#    list(whole_season = modelframe, model = model)
#    list(results = results[!(results$unplayed), ], model = model)

    strength <- coef(model)
    apm <- eppm(strength, draw_par, home_par)
    sched_list<- sched.s(league, season, results, exp(strength), apm, draw_par, home_par)
    ordering <- order(strength)
#    ss <- sched.s(results[!results$unplayed,], s, exp(home_par))
#    ss <- sapply(ss, function(x) x$sched.s)

    standard_table <- read.csv(paste0(dirname, "leagueTable.csv"), as.is = TRUE)
    row.names(standard_table) <- teamNames[as.character(standard_table$teamId)]
    longNames <- longNames[as.character(standard_table$teamId)]
    standard_table $ long_names <- longNames
    standard_table <- standard_table[, c(7, 3:6)]
    names(standard_table) <- c("longnames", "Pld", "GD", "Pts", "rank")
    M_max <- max(standard_table $ Pld)
    standard_table <- standard_table[names(apm), ]
    aPts <- round(M_max * apm, 1)
    ordering <- order(aPts, standard_table$GD, decreasing = TRUE)
    apm <- apm[ordering]
    aPts <- aPts[ordering]
    standard_table <- standard_table[names(aPts), ]
    sched <- aPts - (standard_table$Pts + apm * (M_max - standard_table$Pld))
    res <- data.frame(sched = sched, aPts = aPts)
    res <- cbind(1:nrow(res), res)
    names(res)[1] <- "rank+"
    names(res)[3] <- paste0("Pts|", M_max)
    result <- cbind(standard_table[row.names(res),], res)
    if (is.null(outfile)) outfile <- paste0(league, "/", season, "/alt3.csv")
    write.csv(result, file = outfile)

    require(ggplot2)


    plot_schedule_strengths(sched_list = sched_list)



    return(result)





#    list(results = results[!(results$unplayed), ], s = s, gam = home_par)


}

# temp <- alt3("england-premier-league", prior_weight = 2)

sched.s <- function(league, season = 2017, results, strengths, apm, draw_par, home_par){
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
        sched <-  apm[i] - expected.points
        res <- data.frame(HA = results$HA,
                   vs = opponent,
                   played = 1 - (results $ unplayed),
                   sched = round(sched, 2),
                   cumulative = round(cumsum(sched), 2))
        write.csv(res, file = paste0(dirname, "/", i, ".csv"))
        res
    }
    result <- lapply(names(strengths), ssi)
    names(result) <- names(strengths)
    result
}

plot_schedule_strengths <- function(league = "england-premier-league",
                                    season = 2017, sched_list) {
    require(ggplot2)
    teams <- read.csv(paste(league, season, "namesOfTeams.csv", sep = "/"))
    plot_sched <- function(schedule, team, matches_played) {
        team_fullname <- teams$short_name[teams$abbrev == team]
        dirname <- paste("../../_includes/leagues",
                              league, "schedule-strengths/", sep = "/")
#        dirname <- paste(league, season, "schedule-strengths/", sep = "/")
#        png(file = paste0(dirname, team, ".png"), width = 480, height = 950)
        svg(file = paste0(dirname, team, ".svg"), width = 4, height = 10)
        scalemax <- ceiling(max(abs(schedule $ cumulative)))
        n_played <- sum(schedule $ played)
        plot_the_now_line <- (n_played < 38) &&
            (cumsum(schedule $ played)[1:n_played] == 1:n_played)
        the_plot <- ggplot(schedule, aes(x = cumulative, y = 1:38)) +
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
            scale_color_hue(l = 40, c = 35) +
            geom_label(aes(x = 0, y = -0.1, label = "Start of season"), fill = "white") +
            geom_label(aes(x = 0, y = 39.1, label = "End of season"), fill = "white") +
            geom_label(aes(x = 0, y = 19.5, label = "Halfway"),
                       color = "grey", fill = "white") +
            geom_hline(yintercept = -0.1, linetype = "solid",
                color = "black", size = 1) +
            geom_hline(yintercept = 39.1, linetype = "solid",
                color = "black", size = 1) +
            annotate("text", label = "dot",
                     x = 0, y = 39.4, size = 1,
                     colour = "#880000") +
            geom_hline(yintercept = 19.5, linetype = "dashed",
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
                 subtitle =
#               "Schedule strength (points adjustment \u2014 cumulative)",
               "Schedule strength (cumulative)",
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
        include.text <- paste0("{% include leagues/", league, "/schedule-strengths/", abbrev[i], ".svg %}")
        tx2  <- gsub(pattern = "include-svg-file-here", replace = include.text, x = tx)
        tx3  <- gsub(pattern = "shortname", replace = shortname[i], x = tx2)
        tx4  <- gsub(pattern = "abbrev", replace = abbrev[i], x = tx3)
        writeLines(tx4, con = paste0(dirname, "/schedule-strengths/", abbrev[i], ".md"))
    }
}


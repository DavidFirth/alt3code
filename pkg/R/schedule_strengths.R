sched.s <- function(league, season, results, strengths, draw_par) {
##  results should be only those matches played
    dirname <- paste(league, season, "schedule-strengths", sep = "/")
    ssi <- function(i) {
        results <- results[results$homeTeam == i |
                           results$awayTeam == i, ]
        results$HA <- "H"
        results$HA[results$awayTeam == i] <- "A"
        results$Pts <- 3 * (results$FTR == results$HA) + 1 * (results$FTR == "D")
        results$GD <- (results$FTHG - results$FTAG) *
            ((results$HA == "H") - (results$HA == "A"))
        results$GS <- results$FTHG * (results$HA == "H") + results$FTAG * (results$HA == "A")
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
                   GS = results $ GS,
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
            all(cumsum(schedule $ played)[1:n_played] == (1:n_played))
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
            geom_label(aes(x = 0, y = -0.1, label = "Start of season"),
                       fill = "white") +
            geom_label(aes(x = 0, y = nweeks + 1.1,
                           label = "End of season"),
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
                geom_hline(yintercept = 0.5 + n_played,
                           linetype = "dashed",
                           color = "green", size = 1)
        }
        the_plot <- the_plot +
            geom_vline(xintercept = 0, linetype = "dashed",
                       color = "grey", size = 1) +
            scale_y_reverse(expand = c(0, 0.7)) +
            theme(axis.title.y = element_blank(),
                  axis.text.y = element_blank(),
                  axis.ticks.y = element_blank(),
                  axis.title.x = element_text(color = "darkgrey",
                                                                 size = 10)) +
            scale_x_continuous(position = "top", limits = c(-scalemax, scalemax)) +
            labs(title = team_fullname,
                 subtitle = "Schedule strength (cumulative)",
                 x = " <<<< easier <<<<            >>>> harder >>>>") +
            theme(plot.title = element_text(face="bold", size = 16),
                  plot.margin = margin(0, 0, 0, 0, "cm"))
        the_plot $ layers = rev(the_plot $ layers)
        the_plot <- the_plot +
            scale_color_manual(values=c("#880000", "#000088")) +
            theme(
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

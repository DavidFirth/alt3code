webify3 <- function(league, season) {
    outfile <- paste0("../../_includes/leagues/", league,
                      "/alt3-table.html")
    league_table <- read.csv(paste0(league, "/", season, "/alt3.csv"))

    substantial_imbalance <- abs(league_table $ ePld - league_table $ Pld) > 0.99
    very_tough_schedule <- league_table $ ePld - league_table $ Pld < -0.99
    very_easy_schedule <- league_table $ ePld - league_table $ Pld > 0.99
    extremely_tough_schedule <- league_table $ ePld - league_table $ Pld < -1.99
    extremely_easy_schedule <- league_table $ ePld - league_table $ Pld > 1.99
    slightly_tough_schedule <- (league_table $ ePld - league_table $ Pld < 0) &
        !very_tough_schedule
    slightly_easy_schedule <- (league_table $ ePld - league_table $ Pld > 0) &
        !very_easy_schedule

    league_table $ arrows <- ""
    league_table <- within(league_table, {
        arrows[rank. - rank > 0.5] <- "downsvg"
        arrows[rank. - rank > 2.5] <- "Downsvg"
        arrows[rank - rank. > 0.5] <- "upsvg"
        arrows[rank - rank. > 2.5] <- "Upsvg"
    })
    league_table <- cbind(league_table[, 1:6], league_table[, 10], league_table[, 7:9])
    ht <- huxtable::as_hux(league_table, add_colnames = TRUE)
    huxtable::number_format(ht)[1,10] <- '%2.0f'
    names(ht)[1] <- "team"
    ht[1,1] <- ht[1,2] <- ht[1,6] <- ht[1,7] <- ht[1, 8] <- ""
    ht[1, 3] <- "Pld"
    ht[1, 10] <- gsub("\\.", "|", colnames(ht)[10])

    ht$GD <- gsub("-", "minus", ht$GD)
    team_urls <- paste0("/leagues/", league, "/", "schedule-strength-",
                        ht$team[2:nrow(ht)])
    for (i in 1:nrow(ht)) {
        ht$team[i] <- gsub("^", paste0('opena', team_urls[i - 1],
                                        'closetag'), ht$team[i])
        ht$team[i] <- gsub("$", "closea", ht$team[i])
        }
    for (i in 1:nrow(ht)) {
        ht$longnames[i] <- gsub("^", paste0('opena', team_urls[i - 1],
                                        'closetag'), ht$longnames[i])
        ht$longnames[i] <- gsub("$", "closea", ht$longnames[i])
        }
    huxtable::bold(ht)[1,] <- TRUE
    huxtable::bold(ht)[1, 3:4] <- FALSE
    huxtable::bold(ht)[, 1:2] <- TRUE
    huxtable::bold(ht)[, 5] <- TRUE
    huxtable::bold(ht)[, 8] <- TRUE
    huxtable::bold(ht)[, 10] <- TRUE

    huxtable::background_color(ht)[c(FALSE, very_tough_schedule), 8:10] <- "#ddffdd"
    huxtable::background_color(ht)[c(FALSE, very_easy_schedule), 8:10] <- "#ffdddd"
    huxtable::bold(ht)[c(FALSE, extremely_tough_schedule), 9] <- TRUE
    huxtable::bold(ht)[c(FALSE, extremely_easy_schedule), 9] <- TRUE

    huxtable::bottom_border(ht)[1, ] <- FALSE
    huxtable::align(ht)[, 3] <- 'right'
    huxtable::align(ht)[, 4:10]       <- 'right'
    huxtable::align(ht)[, 6:8]       <- 'center'
    huxtable::right_padding(ht)      <- 2
    huxtable::left_padding(ht)       <- 2
    huxtable::right_padding(ht)[, 5] <- 10
    huxtable::right_padding(ht)[, 10] <- 4
    huxtable::left_padding(ht)[, 10] <- 4
    huxtable::right_padding(ht)[, 1] <- 4
    huxtable::right_padding(ht)[, 2] <- 4
    huxtable::left_padding(ht)[, 1] <- 3
    huxtable::left_padding(ht)[, 2] <- 4
    huxtable::left_padding(ht)[, 3] <- 0
    huxtable::right_padding(ht)[, 3] <- 0
    huxtable::left_padding(ht)[, 4] <- 2
    huxtable::left_padding(ht)[, 9] <- 0
    huxtable::left_padding(ht)[, 7] <- 2
    huxtable::right_padding(ht)[, 7] <- 2
    huxtable::left_padding(ht)[, 8] <- 4
    huxtable::bottom_padding(ht) <- 2
    huxtable::top_padding(ht) <- 2
    huxtable::number_format(ht)[, c(3:6, 8)] <- 0
    huxtable::number_format(ht)[, 10] <- "%.2f"
    huxtable::number_format(ht)[-1, 9] <- "%.1f"
    ht <- rbind(c('', '', 'standard table', '', '', '', '', 'alt-3', '', ''), ht)
    huxtable::colspan(ht)[1, 3] <- 4
    huxtable::align(ht)[1, 3] <- 'center'
    huxtable::text_color(ht)[1, 3] <- "#666666"
    huxtable::background_color(ht)[1, 3:6] <- "gainsboro"
    huxtable::colspan(ht)[1, 8] <- 3
    huxtable::align(ht)[1, 8] <- 'center'
    huxtable::text_color(ht)[1, 8] <- "#000055"
    huxtable::background_color(ht)[1, 8:10] <- "#ffa74d"
    huxtable::background_color(ht)[3:nrow(ht), 7] <- "#000055"
    huxtable::ht <- set_col_width(ht, 7, "20px")
    huxtable::background_color(ht)[2, ] <- "#000055"  ## a grey column-header line
    huxtable::text_color(ht)[2, 3] <- "#ffffff"
    huxtable::text_color(ht)[2, 3:4] <- "#aaaaaa"
    huxtable::text_color(ht)[2, c(5, 9:10)] <- "#ffffff"
    huxtable::background_color(ht)[3:nrow(ht), 3:6] <- "gainsboro"
    huxtable::text_color(ht)[3:nrow(ht), 3:6] <- "#666666"

    ht <- huxtable::set_bottom_border(ht, nrow(ht), everywhere, 4)
    ht <- huxtable::set_bottom_border_color(ht, nrow(ht), c(1:6, 8:10), "lightgrey")
    ht <- huxtable::set_bottom_border_color(ht, nrow(ht), 7, "#000055")
    ht <- huxtable::set_bottom_border(ht, c(3:(nrow(ht) - 1)), c(1:6, 8:10), 2)
    ht <- huxtable::set_bottom_border(ht, c(3:(nrow(ht) - 1)), 7, 2)
    ht <- huxtable::set_bottom_border_color(ht, c(3:(nrow(ht) - 1)), 3:6, "#cccccc")
    ht <- huxtable::set_bottom_border_color(ht, c(2:(nrow(ht) - 1)), 7, "#000055")
    ht <- huxtable::set_bottom_border_color(ht, c(3:(nrow(ht) - 1)), c(1:2, 8:10), "#ffa74d")
    ht <- huxtable::set_bottom_border(ht, 1, everywhere, 2)
    ht <- huxtable::set_bottom_border_color(ht, 1, 3:6, "gainsboro")
    ht <- huxtable::set_bottom_border_color(ht, 1, 8:10, "#ffa74d")
    ht <- huxtable::set_bottom_border_color(ht, 1, c(1:2, 7), "#ffffff")

    timestamp <- paste0('<p style=\"font-size: 80%;\"><i>Table written: ',
                        format(Sys.time(),
                               "%A %d %B %Y at %X"),
                        " (London time)</i> <br>",
                        "If this looks out of date, please try ",
                        "refreshing the page in your browser.",
                        "</p>"
                        )

    ht$V9 <- gsub("minus0.0", "0.0", ht$V9)
    html <- to_html(ht)
    html <- gsub("margin-top: 2em", "margin-top: 1em", html)
    html <- gsub("margin-bottom: 2em", "margin-bottom: 0em", html)
    html <- gsub("minus", "&minus;", html)
    html <- gsub("opena", '<a href=\"', html)
    html <- gsub("closetag", '\">', html)
    html <- gsub("closea", "</a>", html)
    sched_tooltip <- paste0("SCHEDULE STRENGTH\n",
                       "  + : fixtures to date relatively hard\n",
                       "  &minus; : fixtures to date relatively easy")

    html <- gsub("Upsvg",
      '<img width=\"12px\" src=\"/assets/images/up2.svg\" style=\"margin-bottom: 2px;\">',
      html)
    html <- gsub("upsvg",
      '<img width=\"12px\" src=\"/assets/images/up.svg\" style=\"margin-bottom: 2px;\">',
      html)
    html <- gsub("Downsvg",
      '<img width=\"12px\" src=\"/assets/images/down2.svg\" style=\"margin-bottom: 2px;\">',
      html)
    html <- gsub("downsvg",
      '<img width=\"12px\" src=\"/assets/images/down.svg\" style=\"margin-bottom: 2px;\">',
      html)
    html <- gsub("50%", "100%", html)  ## for the table width
    html <- gsub("<th", "<td", html)
    html <- gsub("/th>", "/td>", html)

    cat(c(timestamp, html), file = outfile)

    }



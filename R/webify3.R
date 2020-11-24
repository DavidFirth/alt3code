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

    require(huxtable)



    league_table $ arrows <- ""
    league_table <- within(league_table, {
        arrows[rank. - rank > 0.5] <- "downsvg"
        arrows[rank. - rank > 2.5] <- "Downsvg"
        arrows[rank - rank. > 0.5] <- "upsvg"
        arrows[rank - rank. > 2.5] <- "Upsvg"
    })
    league_table <- cbind(league_table[, 1:6], league_table[, 10], league_table[, 7:9])
    ht <- as_hux(league_table, add_colnames = TRUE)
    number_format(ht)[1,10] <- '%2.0f'
    names(ht)[1] <- "team"
    ht[1,1] <- ht[1,2] <- ht[1,6] <- ht[1,7] <- ht[1, 8] <- ""
    ht[1, 3] <- "Pld"
    ht[1, 10] <- gsub("\\.", "|", colnames(ht)[10])
#    ht$sched <- gsub("-", "minus", ht$sched)
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
    bold(ht)[1,]           <- TRUE
    bold(ht)[1, 3:4] <- FALSE
    bold(ht)[, 1:2]           <- TRUE
    bold(ht)[, 5]           <- TRUE
    bold(ht)[, 8]           <- TRUE
    bold(ht)[, 10]           <- TRUE

#    bold(ht)[c(FALSE, substantial_imbalance), 9] <- TRUE

    background_color(ht)[c(FALSE, very_tough_schedule), 8:10] <- "#ddffdd"
    background_color(ht)[c(FALSE, very_easy_schedule), 8:10] <- "#ffdddd"
    bold(ht)[c(FALSE, extremely_tough_schedule), 9] <- TRUE
    bold(ht)[c(FALSE, extremely_easy_schedule), 9] <- TRUE

 #   text_color(ht)[c(FALSE, slightly_tough_schedule), 9] <- "#669966"
 #   text_color(ht)[c(FALSE, slightly_easy_schedule), 9] <- "#996666"


    bottom_border(ht)[1, ] <- FALSE
    align(ht)[, 3] <- 'right'
    align(ht)[, 4:10]       <- 'right'
    align(ht)[, 6:8]       <- 'center'
    right_padding(ht)      <- 2
    left_padding(ht)       <- 2
    right_padding(ht)[, 5] <- 10
    right_padding(ht)[, 10] <- 4
    left_padding(ht)[, 10] <- 4
    right_padding(ht)[, 1] <- 4
    right_padding(ht)[, 2] <- 4
    left_padding(ht)[, 1] <- 3
    left_padding(ht)[, 2] <- 4
    left_padding(ht)[, 3] <- 0
    right_padding(ht)[, 3] <- 0
    left_padding(ht)[, 4] <- 2
    left_padding(ht)[, 9] <- 0
    left_padding(ht)[, 7] <- 2
    right_padding(ht)[, 7] <- 2
    left_padding(ht)[, 8] <- 4
    bottom_padding(ht) <- 2
    top_padding(ht) <- 2
    number_format(ht)[, c(3:6, 8)] <- 0
    number_format(ht)[, 10] <- "%.2f"
    number_format(ht)[-1, 9] <- "%.1f"
    ht <- rbind(c('', '', 'standard table', '', '', '', '', 'alt-3', '', ''), ht)
    colspan(ht)[1, 3] <- 4
    align(ht)[1, 3] <- 'center'
    text_color(ht)[1, 3] <- "#666666"
    background_color(ht)[1, 3:6] <- "gainsboro"
    colspan(ht)[1, 8] <- 3
    align(ht)[1, 8] <- 'center'
    text_color(ht)[1, 8] <- "#000055"
    background_color(ht)[1, 8:10] <- "#ffa74d"
    background_color(ht)[3:nrow(ht), 7] <- "#000055"
    ht <- set_col_width(ht, 7, "20px")
    ## some alternatives "#FFD4B8" "powderblue"  "lightcyan"
    background_color(ht)[2, ] <- "#000055"  # "gainsboro"  ## a grey column-header line
    text_color(ht)[2, 3] <- "#ffffff"
    text_color(ht)[2, 3:4] <- "#aaaaaa"
    text_color(ht)[2, c(5, 9:10)] <- "#ffffff"
    background_color(ht)[3:nrow(ht), 3:6] <- "gainsboro"
    text_color(ht)[3:nrow(ht), 3:6] <- "#666666"
   # text_color(ht)[, 6] <- "darkgrey"
#    ht <- set_top_border(ht, 1, 8:10, 4)
#    ht <- set_top_border_color(ht, 1, 8:10, "#000044")
#    ht <- set_top_border(ht, 1, 4:6, 4)
#    ht <- set_top_border_color(ht, 1, 4:6, "lightgrey")
    ht <- set_bottom_border(ht, nrow(ht), everywhere, 4)
    ht <- set_bottom_border_color(ht, nrow(ht), c(1:6, 8:10), "lightgrey") #  "#000055"
    ht <- set_bottom_border_color(ht, nrow(ht), 7, "#000055")
#    ht <- set_bottom_border(ht, c(3:21), c(1:6, 8:10), 2)
#    width(ht) <- 1.00
#    font_size(ht) <- 11
#    row_height(ht) <- 9
    ht <- set_bottom_border(ht, c(3:(nrow(ht) - 1)), c(1:6, 8:10), 2)
    ht <- set_bottom_border(ht, c(3:(nrow(ht) - 1)), 7, 2)
    ht <- set_bottom_border_color(ht, c(3:(nrow(ht) - 1)), 3:6, "#cccccc")
    ht <- set_bottom_border_color(ht, c(2:(nrow(ht) - 1)), 7, "#000055")
    ht <- set_bottom_border_color(ht, c(3:(nrow(ht) - 1)), c(1:2, 8:10), "#ffa74d")
    ht <- set_bottom_border(ht, 1, everywhere, 2)
    ht <- set_bottom_border_color(ht, 1, 3:6, "gainsboro")
    ht <- set_bottom_border_color(ht, 1, 8:10, "#ffa74d")
    ht <- set_bottom_border_color(ht, 1, c(1:2, 7), "#ffffff")
#    ht <- set_bottom_border(ht, c(6:7, 19), c(1:6, 8:10), 2)
#    ht <- set_bottom_border_color(ht, c(6:7, 19), c(1:6, 8:10), "grey")
    timestamp <- paste0('<p style=\"font-size: 80%;\"><i>Table written: ', format(Sys.time(), "%A %d %B %Y at %X"), " (London time)</i> <br>If this looks out of date, please try refreshing the page in your browser.</p>")
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
#    html <- gsub(">sched", paste0(' title="', sched_tooltip, '">sched'), html)
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

#    ##  Now update the key file, too
#    tx  <- readLines("key_template.md")
#    tx <- gsub(pattern = "thisleague", replace = league, x = tx)
#    writeLines(tx, con = paste0("../../_includes/leagues/", league, "/key.md"))

    }

## Local Variables:
## ess-r-package-info: ("alt3sources" . "/data/david/work/football/alt3sources")
## End:

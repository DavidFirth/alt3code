webify <- function(league, season = 2017) {
    outfile <- paste0("../../_includes/leagues/", league, ".html")
    league_table <- read.csv(paste0(league, "/", season, "/alt3.csv"))
    nplayed <- max(league_table $ Pld)
    require(huxtable)
    league_table$sched <- sprintf("%.1f", league_table$sched)
    league_table $ arrows <- ""
    league_table <- within(league_table, {
        arrows[rank. - rank > 0.5] <- "downsvg"
        arrows[rank. - rank > 2.5] <- "down2svg"
        arrows[rank - rank. > 0.5] <- "upsvg"
        arrows[rank - rank. > 2.5] <- "up2svg"
    })
    league_table <- cbind(league_table[, 1:6], league_table[, 10], league_table[, 7:9])
    ht <- add_colnames(as_hux(league_table))
    names(ht)[1] <- "team"
    ht[1,1] <- ht[1,2] <- ht[1,6] <- ht[1,7] <- ht[1, 8] <- ""
    ht[1, 3] <- "P"
    ht[1, 10] <- gsub("\\.", "|", ht[1, 10])

    ht$sched <- gsub("-", "minus", ht$sched)
    ht$GD <- gsub("-", "minus", ht$GD)
    team_urls <- paste0("/leagues/", league, "/", "/schedule-strength-",
                        ht$team[2:nrow(ht)])
    for (i in 2:nrow(ht)) {
        ht$team[i] <- gsub("^", paste0('opena', team_urls[i - 1],
                                        'closetag'), ht$team[i])
        ht$team[i] <- gsub("$", "closea", ht$team[i])
        }
    for (i in 2:nrow(ht)) {
        ht$longnames[i] <- gsub("^", paste0('opena', team_urls[i - 1],
                                        'closetag'), ht$longnames[i])
        ht$longnames[i] <- gsub("$", "closea", ht$longnames[i])
        }
    for (i in 2:nrow(ht)) {
        ht$sched[i] <- gsub("^", paste0('opena', team_urls[i - 1],
                                        'closetag'), ht$sched[i])
        ht$sched[i] <- gsub("$", "closea", ht$sched[i])
        }
    bold(ht)[1,]           <- TRUE
    bold(ht[1, 9]) <- FALSE
    bold(ht)[, 1:2]           <- TRUE
    bold(ht)[, 8]           <- TRUE
    bold(ht)[, 10]           <- TRUE
    bottom_border(ht)[1, ] <- FALSE
    align(ht)[, 3:10]       <- 'right'
    align(ht)[, 6:8]       <- 'center'
    right_padding(ht)      <- 2
    left_padding(ht)       <- 2
    right_padding(ht)[, 5] <- 10
    right_padding(ht)[, 10] <- 3
    left_padding(ht)[, 10] <- 4
    right_padding(ht)[, 8] <- 0
    right_padding(ht)[, 1] <- 4
    right_padding(ht)[, 2] <- 4
    left_padding(ht)[, 1] <- 2
    left_padding(ht)[, 2] <- 4
    left_padding(ht)[, 3] <- 2
    left_padding(ht)[, 9] <- 0
    left_padding(ht)[, 8] <- 1
    left_padding(ht)[, 7] <- 3
    right_padding(ht)[, 6:7] <- 1
    ht <- set_col_width(ht, 7, "24px")
    number_format(ht)[, c(3:6, 8)] <- 0
    number_format(ht)[, 9:10] <- "%-1.1f"
    ht <- rbind(c('', '', '', '', '', '', 'alt-3', '', '', ''), ht)
    colspan(ht)[1, 7] <- 4
    align(ht)[1, 7] <- 'center'
    background_color(ht)[, 7:10] <- "lemonchiffon"
    ## some alternatives "#FFD4B8" "powderblue"  "lightcyan"
    background_color(ht)[2, ] <- "gainsboro"  ## a grey column-header line
    text_color(ht)[, 6] <- "darkgrey"
    ht <- set_top_border(ht, 1, 7:10, 4)
    ht <- set_bottom_border(ht, 22, everywhere, 4)
    ht <- set_bottom_border_color(ht, 22, everywhere, "lightgrey")
    width(ht) <- 1.00
    font_size(ht) <- 11
    row_height(ht) <- 9
    ht <- set_bottom_border(ht, c(3:5, 8:18, 20:21), everywhere, 2)
    ht <- set_bottom_border_color(ht, c(3:5, 8:18, 20:21), everywhere, "gainsboro")
    ht <- set_bottom_border(ht, c(6:7, 19), everywhere, 2)
    ht <- set_bottom_border_color(ht, c(6:7, 19), everywhere, "grey")
    timestamp <- paste0('<p style=\"font-size: 80%;\"><i>Table written: ', Sys.time(), " (London time).</i> <br>If this looks out of date, please try refreshing the page in your browser.</p>")
    html <- to_html(ht)
    html <- gsub("minus", "&minus;", html)
    html <- gsub("opena", '<a href=\"', html)
    html <- gsub("closetag", '\">', html)
    html <- gsub("closea", "</a>", html)
    html <- gsub("up2svg",
      '<img width=\"11\" src=\"/assets/images/up2.svg\" style=\"margin-bottom: 2px;\">',
      html)
    html <- gsub("upsvg",
      '<img width=\"11\" src=\"/assets/images/up.svg\" style=\"margin-bottom: 2px;\">',
      html)
    html <- gsub("down2svg",
      '<img width=\"11\" src=\"/assets/images/down2.svg\" style=\"margin-bottom: 2px;\">',
      html)
    html <- gsub("downsvg",
      '<img width=\"11\" src=\"/assets/images/down.svg\" style=\"margin-bottom: 2px;\">',
      html)
    cat(c(html, timestamp), file = outfile)

    ##  Now update the key file, too
    tx  <- readLines("key_template.md")
    tx <- gsub(pattern = "thisleague", replace = league, x = tx)
    tx2  <- gsub(pattern = "nplayed", replace = as.character(nplayed), x = tx)
    writeLines(tx2, con = paste0("../../_includes/leagues/", league, "-key.md"))

    }

## Local Variables:
## ess-r-package-info: ("alt3sources" . "/data/david/work/football/alt3sources")
## End:

plot_rates <- function(rates) {
    ## Rates is a data frame in 3 columns, all rounded to 2 decimal places
    ## Columns are away_rate, home_rate and alt3_rate

    ## Specify some graphics parameters
    colour_values = c("black", "black", "black", "blue", "red")
    hjust_values <- c(0.3, 0.3, 0.3, 0.7, 0.3)
    vjust_values <- c(0.5, 0.5, 0.5, 0.3, 0.7)
    nudge_x_values <- c(0.03, -0.03, -0.03, -0.03, 0.03)
    nudge_y_values <- c(0.03, -0.03,  0.03,  0.03, -0.03)
    names(colour_values) <-
        names(hjust_values) <-
        names(vjust_values) <-
        names(nudge_x_values) <-
        names(nudge_y_values) <- c("zero", "three", "equal", "home_better", "away_better")
    alt3_colour <- "orange"

    ## Remove teams that have not yet played both at home and away
    rates <- na.omit(rates)

    ## Compute 5 groups of teams --- home record better, away record better,
    ## equal at zero points, equal at 3 per metch, and other equal
    group <- rep("equal", nrow(rates))
    group <- ifelse(rates$alt3_rate == 0, "zero", group)
    group <- ifelse(rates$alt3_rate == 3, "three", group)
    group <- ifelse(rates$home_rate > rates$away_rate, "home_better", group)
    group <- ifelse(rates$away_rate > rates$home_rate, "away_better", group)

    teamnames <- row.names(rates)

    ## Set up the plot frame
    g <- ggplot2::ggplot(rates,
                         ggplot2::aes(x = away_rate,
                                      y = home_rate,
                                      label = teamnames))
    g <- g + ggplot2::scale_x_continuous(breaks = c(0, 1, 2, 3),
                                         labels = c("", "1", "2", "3"),
                                         limits = c(0, 3))
    g <- g + ggplot2::scale_y_continuous(breaks = c(0, 1, 2, 3),
                                         labels = c("", "1", "2", "3"),
                                         limits = c(0, 3))
    g <- g + ggplot2::theme_minimal()
    g <- g + ggplot2::theme(axis.title.x=element_blank(),
                            axis.title.y = ggplot2::element_blank(),
                            axis.text.x = ggplot2::element_text(angle = -45,
                                                                vjust = 0.8,
                                                                hjust = 0,
                                                                size = 16),
                            axis.text.y = ggplot2::element_text(angle = -45,
                                                                vjust = 0.9,
                                                                hjust = 0.7,
                                                                size = 16),
                            axis.ticks.x = ggplot2::element_blank(),
                            axis.ticks.y = ggplot2::element_blank(),
                            plot.margin = grid::unit(c(0.8, 0.8, 0,0), "cm"))

    ## Draw the alt-3 centre line and its associated labels
    g <- g + ggplot2::geom_segment(data = rates,
                                   mapping = ggplot2::aes(x = 0, y = 0, xend = 3, yend = 3),
                                   size = 1,
                                   color = alt3_colour)
    g <- g + ggplot2::geom_point(x = -0.075,
                                 y = -0.075,
                                 shape = 21,
                                 colour = alt3_colour,
                                 fill = "white",
                                 size = 9,
                                 stroke = 0.7)
    g <- g + ggplot2::geom_text(x = -0.075,
                                y = -0.075,
                                label = "0",
                                color = alt3_colour,
                                angle = -45,
                                size = 6)
    g <- g + ggplot2::geom_point(x = 3.075,
                                 y = 3.075,
                                 shape = 21,
                                 colour = alt3_colour,
                                 fill = "white",
                                 size = 9,
                                 stroke = 0.7)
    g <- g + ggplot2::geom_text(x = 3.075,
                                y = 3.075,
                                label = "3",
                                color = alt3_colour,
                                angle = -45,
                                size = 6)
    g <- g +  ggplot2::geom_text(x = 3.63,
                                 y = 2.62,
                                 label = "Rate = Pts/ePld",
                                 color = alt3_colour,
                                 angle = -45,
                                 size = 6)
    g <- g +  ggplot2::geom_text(x = 3.52,
                                 y = 2.54,
                                 label = "(the alt-3 table)",
                                 color = alt3_colour,
                                 angle = -45,
                                 size = 3)

    ## Plot the home and away rates, labelled and connected to the alt-3 axis
    g <- g + ggplot2::geom_point(colour = colour_values[group])
    if (any(group == "zero")) {
        g <- g + ggplot2::geom_text(ggplot2::aes(x = 0, y = 0,
                                                 label =
                                                     paste(sort(teamnames[group == "zero"]),
                                                           collapse = " ")),
                                    angle = -45,
                                    vjust = 0,
                                    nudge_y = nudge_y_values["zero"],
                                    nudge_x = nudge_x_values["zero"],
                                    size = 3)
    }
    if (any(group == "three")) {
        g <- g + ggplot2::geom_text(ggplot2::aes(x = 3, y = 3,
                                                 label =
                                                     paste(sort(teamnames[group == "three"]),
                                                           collapse = " ")),
                                    angle = -45,
                                    vjust = 1,
                                    nudge_y = nudge_y_values["three"],
                                    nudge_x = nudge_x_values["three"],
                                    size = 3)
    }
    remaining_teams <- group %in% c("equal", "home_better", "away_better")
    remaining_labels <- teamnames
    remaining_labels[!remaining_teams] <- ""
    g <- g + ggrepel::geom_text_repel(
                 ggplot2::aes(label = remaining_labels),
                 angle = -45,
                 colour = colour_values[group],
                 hjust = hjust_values[group],
                 nudge_x = nudge_x_values[group],
                 nudge_y = nudge_y_values[group],
                 vjust = vjust_values[group],
                 min.segment.length = 5,
                 size = 3,
                 force_pull = 10,
                 force = 0.1,
                 box.padding = 0.07)
    g <- g + ggplot2::geom_segment(ggplot2::aes(x = away_rate,
                                       y = home_rate,
                                       xend = alt3_rate,
                                       yend = alt3_rate),
                                   colour = colour_values[group])

    ## Add surrounding axis labels and annotation
    g <- g + ggplot2::annotation_custom(
                          grob = grid::textGrob(label = "Home\npoints\nrate",
                                                rot = -45,
                                                x = grid::unit(-0.2, "npc"),
                                                y = grid::unit(0.5, "npc"),
                                                gp = grid::gpar(fontsize = 20)))
    g <- g + ggplot2::annotation_custom(
                          grob = grid::textGrob(label = "Away\npoints\nrate",
                                                rot = -45,
                                                x = grid::unit(+0.5, "npc"),
                                                y = grid::unit(-0.2, "npc"),
                                                gp = grid::gpar(fontsize = 20)))
    g <- g + ggplot2::annotation_custom(
                          grob = grid::textGrob(label = "Blue: Home rate better",
                                                rot = -45,
                                                hjust = 0,
                                                x = grid::unit(0.31, "npc"),
                                                y = grid::unit(1.30, "npc"),
                                                gp = grid::gpar(fontsize = 12,
                                                                col = colour_values[
                                                                    "home_better"])))
    g <- g + ggplot2::annotation_custom(
                          grob = grid::textGrob(label = "Red: Away rate better",
                                                rot = -45,
                                                hjust = 0,
                                                x = grid::unit(0.28, "npc"),
                                                y = grid::unit(1.27, "npc"),
                                                gp = grid::gpar(fontsize = 12,
                                                                col = colour_values[
                                                                    "away_better"])))

    ## Rotate the viewport when outputting the graph
    ## This always seems to give a couple of puzzling warnings --- suppress those.
    suppressWarnings(
        print(g, vp = grid::viewport(width = unit(0.77, "npc"),
                               height = unit(0.77, "npc"),
                               angle = 45,
                               clip = "off")))
}


adjusted_points_rate <- function(log_strength, delta){
    a <- exp(log_strength)
    (3*a + delta * a^(1/3)) /  (a + 1 + delta * a^(1/3))
}


plot_strengths <- function(model){
    coefs <- coef(model)
    delta <- exp(coefs[length(coefs)])
    log_strengths <- rev(sort(coefs[-length(coefs)]))
    apr <- adjusted_points_rate(log_strengths, delta)
    thedata <- data.frame(apr = apr, teamname = names(apr))
    the_plot <- ggplot2::ggplot(thedata, aes(x = 1:length(apr), y = apr)) +
        ggplot2::geom_point() +
        ggplot2::geom_text(aes(label = teamname, angle=45), color = "green",
                  hjust = -0.1, vjust = 0.5, size = 3,
                  show.legend = FALSE) +
        ggplot2::theme_light()
    pdf()
    print(the_plot)
    dev.off()
}

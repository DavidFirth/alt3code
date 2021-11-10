eppm <- function(coefs, draw_par, home_par) {
    alphas <- exp(coefs)
    delta <- exp(draw_par)
    gamma <- exp(home_par)
    pts <- outer(alphas, alphas, function(x, y) {
        (3 * sqrt(gamma) * x + delta * (x * y)^(1/3)) /
            (sqrt(gamma) * x + y / sqrt(gamma) + delta * (x * y)^(1/3) ) +
            (3 * x / sqrt(gamma) + delta * (x * y)^(1/3)) /
                (x / sqrt(gamma) + sqrt(gamma) * y + delta * (x * y)^(1/3))
    })
    diag(pts) <- 0
    rowSums(pts) / (2 * (length(coefs) - 1))
}


eppm <- function(strengths, delta, prior_weight) {
    nTeams <- nrow(strengths)
    pts_home <- outer(strengths[, "home"], strengths[, "away"], function(x, y) {
        (3 * x + delta * (x * y)^(1/3)) /
            (x + y + delta * (x * y)^(1/3) )
    })
    pts_away <- outer(strengths[, "home"], strengths[, "away"], function(x, y) {
        (3 * y + delta * (x * y)^(1/3)) /
            (x + y + delta * (x * y)^(1/3) )
    })
    diag(pts_home) <- 0
    diag(pts_away) <- 0
    rate_home <- (3 * prior_weight * (strengths[, "home"] / (1 + strengths[, "home"]) - 0.5)  + rowSums(pts_home)) / (nTeams - 1)
    rate_away <- (3 * prior_weight * (strengths[, "away"] / (1 + strengths[, "away"]) - 0.5)  + colSums(pts_away)) / (nTeams - 1)
    result <- strengths  ## just a container with the right structure
    result[, "home"] <- rate_home
    result[, "away"] <- rate_away
    return(result)
}

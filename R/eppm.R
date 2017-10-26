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

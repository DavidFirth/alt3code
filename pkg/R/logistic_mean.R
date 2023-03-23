## This function gets used for centering the vector of team strengths, if
## the damping prior is not used (ie, has weight zero).  The damping prior
## will otherwise do this specific centering automatically.

logistic_mean <- function(yvec){
    startvalue <- median(yvec) # a sensible starting value for glm
    halves <- rep(0.5, length(yvec))
    coef(glm(halves ~ 1, offset = -yvec, family = quasibinomial, start = startvalue))
}

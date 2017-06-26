# Tools to fit fundamental diagrams


#' Fit Robust Triangular Fundamental Diagram
#'
#' The fundamental diagram (FD) in traffic engineering represents 
#' flow as a function of density.
#'
#' This only fits 2 robust linear models and returns the coefficients. It
#' is not a true triangular fundamental diagram because the fitted models
#' include intercepts, and does not enforce the constraint that low and
#' high density should meet at the cutoff point.
#'
#' @param flow Count of passed vehicles
#' @param occupancy Traffic density between 0 and 1
#' @param cutoff Point separating high and low occupancy
#' @param ... Additional arguments for code{\link[MASS]{rlm}}
#' @return fd list containing fitted FD
#' @export
fd_rlm = function(flow, occupancy, cutoff = 0.15, ...)
{

    congested = occupancy > cutoff

    high = MASS::rlm(flow[congested] ~ occupancy[congested], ...)
    low = MASS::rlm(flow[!congested] ~ occupancy[!congested], ...)

    list(high_occcupancy = coef(summary(high))
         , low_occupancy = coef(summary(low))
         )
}


#' Fit the fundamental diagram to observations
#'
#' The fundamental diagram in traffic engineering represents 
#' flow as a function of density.
#'
#' @param flow Count of passed vehicles
#' @param density Traffic density between 0 and 1
#' @param method Method for curve fitting. Available options: \code{"ls"} for least squares.
#' @param cutoff Point separating high and low density
#' @return fd Object of class \code{fd}.
#' @export
fun_diagram = function(flow, density, method = "ls", cutoff = 0.15)
{

    fitted = switch(method
                    , ls = ls_fd(flow, density, cutoff)
                    )

    out = list(flow = flow
         , density = density
         , method = method
         , cutoff = cutoff
         , fit = fitted
         )
    class(out) = c("fd", "list")
    out
}


plot.fd = function(fd)
{
}


ls_fd = function(flow, density, cutoff)
{
}

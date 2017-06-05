# Tools to fit fundamental diagrams


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

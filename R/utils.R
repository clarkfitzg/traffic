#' Split And Append Results To CSV File
#'
#' x will be split by f and each group will be appended to a directory of
#' csv files named according to f
#'
#' @param x data frame to split
#' @param f factor defining splits
#' @param ... further arguments to split
#' @param dirname character directory, will be created if doesn't exist
#' @return NULL
#' @export
split_write = function(x, f, dirname, ...)
{

    sp = split(x, f, ...)

    yesdir = dir.exists(dirname)

    if(!yesdir) dir.create(dirname)

    allfiles = paste0(dirname, "/", names(sp), ".csv")

    mapply(write.table, sp, allfiles, MoreArgs = list(append = yesdir, col.names = !yesdir, sep = ","))

    NULL

}

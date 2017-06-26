#' Read 30 Second Raw PeMS File
#'
#' @param file Name of file to read
#' @param posix_timestamp Convert the timestamp to POSIXct
#' @param lanes Vector of lanes to read. Others ignored.
#' @param numlanes Total expected number of lanes in file
#' @param nrows Number of rows to read. Use -1 to read all of them.
#' @param ... Additional parameters for read.table
#' @return data.frame
#' @export
read30sec = function(file, posix_timestamp = FALSE, lanes = 1:2
                     , numlanes = 8, nrows = 10000L, ...)
{
    ln = data.frame(number = rep(seq.int(numlanes), each = 3))
    ln$name = paste0(rep(c("flow", "occupancy", "speed"), numlanes), ln$number)
    ln$class = rep(c("integer", "numeric", "integer"), numlanes)
    ln$keep = ln$number %in% lanes
    ln$colname = ifelse(ln$keep, ln$name, "NULL")
    ln$colclass = ifelse(ln$keep, ln$class, "NULL")

    rawdata = read.table(file, header = FALSE, sep = ",", nrows = nrows
        , col.names = c("timestamp", "station", ln$colname)
        , colClasses = c("character", "integer", ln$colclass)
        , ...)

    if(posix_timestamp)
        rawdata$timestamp = as.POSIXct(rawdata$timestamp, format = "%m/%d/%Y %H:%M:%S")

    rawdata
}


#' Extract the minutes 
#'
#' @param ts POSIXct timestamp
extract_minutes = function(ts){
    hours = as.integer(format(ts, "%H"))
    minutes = as.integer(format(ts, "%M"))
    60L * hours + minutes
}


readstation = function(file = "~/data/pems/d04_text_meta_2016_10_05.txt")
{
    station = read.table(file
                         , sep = "\t"
                         , header = TRUE
                         , quote = ""
                         , stringsAsFactors = FALSE
                         )[, c("ID", "Abs_PM", "Fwy", "Dir", "Type")]

    station[, "FwyDir"] = paste0(station[, "Fwy"], station[, "Dir"])

    # Only considering Mainline stations
    station = station[station[, "Type"] == "ML", ]

    station[, "Type"] = NULL
    station[, "Fwy"] = NULL
    station[, "Dir"] = NULL
    station
}

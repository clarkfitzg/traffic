#' Read a 30 second raw PeMS file
#'
#' Takes a conservative approach to cleaning data by only performing the following steps:
#' Occupancy greater than 1 is converted to NA
#'
#' ID is the station id. Name chosen to match metadata files.
#'
#' @param file Name of file to read
#' @param lanes Vector of lanes to read. Others ignored.
#' @param numlanes Total expected number of lanes in file
#' @param nrows Number of rows to read. Use -1 to read all of them.
#' @param ... Additional parameters for read.table
#' @return data.frame
#' @export
read30sec = function(file, lanes = 1:2, numlanes = 8, nrows = 10000L, ...)
{
    ln = data.frame(number = rep(seq.int(numlanes), each = 3))
    ln$name = paste0(rep(c("flow", "occupancy", "speed"), numlanes), ln$number)
    ln$class = rep(c("integer", "numeric", "integer"), numlanes)
    ln$keep = ln$number %in% lanes
    ln$colname = ifelse(ln$keep, ln$name, "NULL")
    ln$colclass = ifelse(ln$keep, ln$class, "NULL")

    rawdata = read.table(file, header = FALSE, sep = ",", nrows = nrows
        , col.names = c("timestamp", "ID", ln$colname)
        , colClasses = c("character", "integer", ln$colclass)
        , ...)

    rawdata$timestamp = as.POSIXct(rawdata$timestamp, format = "%m/%d/%Y %H:%M:%S")

    occupancy = grep("occupancy", colnames(rawdata), value = TRUE)

    occupancy_too_big = rawdata[, occupancy] > 1
    rawdata[, occupancy][occupancy_too_big] = NA

    rawdata
}


#' Split And Append Results To CSV File
#'
#' x will be split by f and each group will be appended to a directory of
#' csv files
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

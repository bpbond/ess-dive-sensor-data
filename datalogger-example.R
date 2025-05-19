
library(tibble)
library(readr)

# Read a raw Campbell datalogger data file; modified from
# https://github.com/COMPASS-DOE/compasstools/blob/main/R/datalogger.R
read_datalogger_file <- function(filename) {

    # Parse line 1 to extract logger and table names
    message("Reading ", filename)
    dat <- read_lines(filename)
    header_split <- strsplit(dat[1], ",")[[1]]
    header_split <- gsub("\"", "", header_split) # remove quotation marks
    format_name <- header_split[1] # first field of row 1
    logger_name <- header_split[2] # second field of row 1
    table_name <- header_split[length(header_split)]
    col_units <- dat[3]

    x <- read_csv(I(dat[-c(1, 3, 4)]), col_types = cols(.default = col_character()))
    myrep <- function(s) paste(rep(s, ncol(x)), collapse = ", ")
    attr(x, "DL_LOGGER") <- myrep(logger_name)
    attr(x, "DL_TABLE") <- myrep(table_name)
    attr(x, "DL_TABLE_FORMAT") <- myrep(format_name)
    attr(x, "DL_COL_UNITS") <- col_units
    return(x)
}

# Write a data frame into the "A&B" format
write_example_file <- function(x, filename) {

    message("Writing header for ", filename)
    att <- attributes(x)
    our_attributes <- grep("^DL_", names(att))
    hdr <- c()
    for(i in our_attributes) {
        hdr <- append(hdr, paste("#", names(att)[i], ",", att[i]))
    }
    write_lines(hdr, filename)
    message("Writing data for ", filename)
    spacer_column <- as.data.frame(matrix(NA, nrow = nrow(x)))
    write_csv(cbind(spacer_column, x), filename, append = TRUE, col_names = TRUE)
}


x <- read_datalogger_file("datalogger_files/Compass_OWC_W_321_ExoTable_20230508000343_short.dat") -> x
write_example_file(x, "test.csv")

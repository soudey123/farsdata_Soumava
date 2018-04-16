#' Reads .csv file
#' @description 
#' This is a generic function reads the .csv file contains the data data from the US National Highway Traffic Safety Administration's
#' system \url{https://www.nhtsa.gov/Data/Fatality-Analysis-Reporting-System-(FARS)} and then create data frame with that data.
#' @param filename as an input character vector
#' @param 
#' @return the data frame dplyr::tbl_df() contains the whole data set or null if the file doesn't exist
#' @import readr
#' @import dplyr
#' @export
#' @example 
#' \dontrun{
#' accident_2013 <- fars_read("./data/accident_2013.csv.bz2")}

fars_read <- function(filename) {
  if(!file.exists(filename))
    stop("file '", filename, "' does not exist")
  data <- suppressMessages({
    readr::read_csv(filename, progress = FALSE)
  })
  dplyr::tbl_df(data)
}

#' This function creats a filename
#' This function returns a STRING based on the year
#' @param year as a numeric operator
#' @return sprintf output STRING fromat based on the year passed as an parameter
#' @example
#' make_filename(2013)
#' [1] "accident_2013.csv.bz2"

make_filename <- function(year) {
  year <- as.integer(year)
  sprintf("accident_%d.csv.bz2", year)
}


#' Reads month and year from accident files
#' @description
#' The function accepts a vector or list of years and returns a list of data
#' frames with MONTH and year columns based on data in "accident_<year>.csv.bz2
#' files. The files need to be located in the working directory.
#' @param years input numeric vector
#' @return tibble or null if the year is invalid
#' @import dplyr
#' @export
#' @examples
#' \dontrun{
#' fars_read_years(2013:2015)
#' fars_read_years(list(2013, 2014))
#' }
fars_read_years <- function(years) {
  lapply(years, function(year) {
    file <- make_filename(year)
    tryCatch({
      dat <- fars_read(file)
      dplyr::mutate(dat, year = year) %>% 
        dplyr::select(MONTH, year)
    }, error = function(e) {
      warning("invalid year: ", year)
      return(NULL)
    })
  })
}

#' Counts number of accidents per month and year
#'
#' Based on the list of years, the function calculates the number of accidents
#' in the US on a monthly basis. The accident files need to be in the working
#' directory, the years can be passed as a list or a vector.
#' @param years a vector or list of years in numeric format
#' @return a tibbol based on the months in rows and selected years in columns cotanining the number of accidents
#' @import dplyr
#' @import tidyr
#' @export
#' @examples
#' \dontrun{
#' fars_summarize_years(2013:2016)
#' }



fars_summarize_years <- function(years) {
  dat_list <- fars_read_years(years)
  dplyr::bind_rows(dat_list) %>% 
    dplyr::group_by(year, MONTH) %>% 
    dplyr::summarize(n = n()) %>%
    tidyr::spread(year, n)
}

#' Plots the accidents on a US state map
#' The function accepts a state number and the year to plot the accidents on a single map
#' State number needs to be exixt in FARS data otherwise the fuction will return invalid state
#' @param state.num The number of a state in the US as used in the FARS data
#' sets. Should be numeric or integer.
#' @param year The year of analysis (numeric or integer)
#' @return a plot of the accidents based on the year and the state number
#' @import dplyr
#' @import maps
#' @import graphics
#' @export
#' @examples 
#' \dontrun{
#' fars_map_state(26, 2013)}

fars_map_state <- function(state.num, year) {
  filename <- make_filename(year)
  data <- fars_read(filename)
  state.num <- as.integer(state.num)
  
  if(!(state.num %in% unique(data$STATE)))
    stop("invalid STATE number: ", state.num)
  data.sub <- dplyr::filter(data, STATE == state.num)
  if(nrow(data.sub) == 0L) {
    message("no accidents to plot")
    return(invisible(NULL))
  }
  is.na(data.sub$LONGITUD) <- data.sub$LONGITUD > 900
  is.na(data.sub$LATITUDE) <- data.sub$LATITUDE > 90
  with(data.sub, {
    maps::map("state", ylim = range(LATITUDE, na.rm = TRUE),
              xlim = range(LONGITUD, na.rm = TRUE))
    graphics::points(LONGITUD, LATITUDE, pch = 46)
  })
}

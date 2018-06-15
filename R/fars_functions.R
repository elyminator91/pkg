#' Import data file
#'
#' @description Read a file and convert into a dataframe
#'    structure
#'
#' @param filename A character string for the file name
#'
#' @return Return a dataframe if file exists, otherwise
#'    throw an error
#'
#' @export
#'
#' @importFrom ("dplyr", "tbl_df")
#' @importFrom ("readr", "read_csv")
#'
#' @examples
#' fars_read("accident_2015.csv.bz2")
fars_read <- function(filename) {
  if(!file.exists(filename))
    stop("file '", filename, "' does not exist")
  data <- suppressMessages({
    readr::read_csv(filename, progress = FALSE)
  })
  dplyr::tbl_df(data)
}

#' Make file name
#'
#' @desciption Print a file name based on user input 'year'
#'
#' @param year An integer representing the 'year' of data
#'
#' @return Prints the data file name
#'
#' @export
#'
#' @examples
#' make_filename(2013)
#' make_filename(2014)
make_filename <- function(year) {
  year <- as.integer(year)
  sprintf("accident_%d.csv.bz2", year)
}

#' Check data years
#'
#' @description Check if year is valid in the data
#'
#' @param years A list of integers for data 'years'
#'
#' @return Return a list of length(year) if data
#'    exists, otherwise print an error warning that
#'    the input 'year' is invalid
#'
#' @importFrom ("dplyr", "mutate", "select")
#'
#' @export
#'
#' @examples
#' fars_read_years(2013:2015)
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

#' Summarise data files
#'
#' @description Summarise the total counts of
#' accidents per month, and presenting it in a data
#' frame with 'year' in columns and 'month' in rows
#'
#' @param years An integer list representing the
#'    years of the accident data
#'
#' @return Return a dataframe containing total number
#'   of accident records per month (rows) per year (cols)
#'
#' @importFrom ("dplyr", "bind_row", "group_by", "summarize")
#' @importFrom ("tidyr", "spread")
#'
#' @export
#'
#' @examples
#' fars_summarize_years(2013:2015)
fars_summarize_years <- function(years) {
  dat_list <- fars_read_years(years)
  dplyr::bind_rows(dat_list) %>%
    dplyr::group_by(year, MONTH) %>%
    dplyr::summarize(n = n()) %>%
    tidyr::spread(year, n)
}

#' Map of accident locations by state and year
#'
#' @description Plot the accident occurence by state and year
#'    onto a map
#'
#' @param state.num An integer for 'state' where accident occur
#' @param year An integet for 'year' of accident
#'
#' @return Return a map showing the locations of the accidents
#'   for the year. If none, return a message that there is no
#'   accidents. If invalid state, throw an error
#'
#' @export
#'
#' @examples
#' fars_map_state(1,2013)
#' fars_map_state(10,2014)
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

#' Read a FARS data file
#'
#' Reads a single Fatality Analysis Reporting System (FARS) CSV file from
#' disk into a tibble. The file is read silently (no progress bar or
#' column-parsing messages) and returned using \code{dplyr::tbl_df}.
#'
#' @param filename A character string giving the path to the CSV file to
#'   read (e.g. \code{"accident_2013.csv.bz2"}).
#'
#' @return A tibble (\code{tbl_df}) containing the contents of the file.
#'
#' @details If \code{filename} does not exist on disk, the function stops
#'   with an error message rather than attempting to read it.
#'
#' @importFrom readr read_csv
#' @importFrom dplyr tbl_df
#'
#' @examples
#' \dontrun{
#' fars_read("accident_2013.csv.bz2")
#' }
#'
#' @export
fars_read <- function(filename) {
  if(!file.exists(filename))
    stop("file '", filename, "' does not exist")
  data <- suppressMessages({
    readr::read_csv(filename, progress = FALSE)
  })
  dplyr::tbl_df(data)
}


#' Construct a FARS data filename for a given year
#'
#' Builds the expected filename for a FARS accident data file corresponding
#' to a given year, following the pattern \code{"accident_<year>.csv.bz2"}.
#' This does not check that the file actually exists.
#'
#' @param year A number or character string representing a year (e.g.
#'   \code{2013} or \code{"2013"}). It is coerced to an integer internally.
#'
#' @return A character string giving the constructed filename, e.g.
#'   \code{"accident_2013.csv.bz2"}.
#'
#' @examples
#' make_filename(2013)
#' make_filename("2014")
#'
#' @export
make_filename <- function(year) {
  year <- as.integer(year)
  sprintf("accident_%d.csv.bz2", year)
}


#' Read FARS data for multiple years
#'
#' For each year supplied, constructs the corresponding FARS filename,
#' reads the file, and extracts the \code{MONTH} and \code{year} columns.
#' If a given year's file cannot be read (e.g. it does not exist or the
#' year is invalid), a warning is issued for that year and \code{NULL} is
#' returned for it instead of stopping execution.
#'
#' @param years A vector of years (numeric or character) to read, e.g.
#'   \code{c(2013, 2014, 2015)}.
#'
#' @return A list, the same length as \code{years}, where each element is
#'   either a tibble with columns \code{MONTH} and \code{year} for that
#'   year, or \code{NULL} if that year's data could not be read.
#'
#' @details This function relies on \code{\link{make_filename}} to build
#'   each file path and \code{\link{fars_read}} to read it. Errors raised
#'   while reading an individual year's file are caught internally (via
#'   \code{tryCatch}) so that one bad year does not stop processing of the
#'   others; a warning naming the invalid year is issued instead.
#'
#' @importFrom dplyr mutate select
#' @importFrom magrittr %>%
#'
#' @examples
#' \dontrun{
#' fars_read_years(c(2013, 2014))
#' }
#'
#' @export
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


#' Summarize FARS accident counts by month and year
#'
#' Reads FARS data for the specified years and produces a summary table of
#' the number of accidents recorded in each month, with one column per
#' year.
#'
#' @param years A vector of years (numeric or character) to summarize,
#'   e.g. \code{c(2013, 2014, 2015)}.
#'
#' @return A tibble with one row per month (\code{MONTH}) and one column
#'   per requested year, where each cell gives the count of accident
#'   records for that month/year combination.
#'
#' @details Internally this calls \code{\link{fars_read_years}} to read
#'   and combine the requested years' data, then groups by year and month
#'   and counts records with \code{dplyr::summarize(n = n())}, before
#'   reshaping to a wide format with \code{tidyr::spread}. Any year that
#'   could not be read (see \code{\link{fars_read_years}}) will result in
#'   \code{NULL} rows being dropped when the list is row-bound.
#'
#' @importFrom dplyr bind_rows group_by summarize
#' @importFrom tidyr spread
#' @importFrom magrittr %>%
#'
#' @examples
#' \dontrun{
#' fars_summarize_years(c(2013, 2014))
#' }
#'
#' @export
fars_summarize_years <- function(years) {
  dat_list <- fars_read_years(years)
  dplyr::bind_rows(dat_list) %>%
    dplyr::group_by(year, MONTH) %>%
    dplyr::summarize(n = n()) %>%
    tidyr::spread(year, n)
}


#' Plot FARS accidents on a state map
#'
#' Reads FARS accident data for a given year and state, and plots the
#' location of each recorded accident as a point on a map of that state.
#'
#' @param state.num A number or character string giving the FARS numeric
#'   state code to plot (e.g. \code{6} for California). It is coerced to
#'   an integer internally.
#' @param year A number or character string giving the year of data to
#'   read, passed to \code{\link{make_filename}}.
#'
#' @return Invisibly returns \code{NULL}. The function is called for its
#'   side effect of drawing a plot; if there are no accidents to plot for
#'   the given state, a message is printed and no plot is drawn.
#'
#' @details The function stops with an error if \code{state.num} is not a
#'   valid state code present in the data (i.e. not in
#'   \code{unique(data$STATE)}). Longitude values greater than 900 and
#'   latitude values greater than 90 are treated as missing (\code{NA})
#'   before plotting, since these are FARS sentinel values for unknown
#'   coordinates.
#'
#' @importFrom dplyr filter
#' @importFrom maps map
#' @importFrom graphics points
#'
#' @examples
#' \dontrun{
#' fars_map_state(6, 2013)
#' }
#'
#' @export
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

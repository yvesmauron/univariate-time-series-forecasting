#' Increment for date sequence
#' 
#' To be improved or removed
#'
#' @param y teime series
#' @param direction direction of the increment
#' @param ... not used
#'
#' @return increment
#' @export
date_increment <- function(y, direction = 'future', ...) {
  fqy <- round(frequency(y))
  str <- switch (as.character(fqy),
    '1' = 'year',
    '4' = 'quarter',
    '12' = 'month',
    '52' = 'week'
  )
  if(direction == 'future') {
    return(paste(1, str))
  } else {
    return(paste(-1, str))
  }
}

#' Decimal to date
#'
#' @param dec_date decimal date
#' @param ... not used
#'
#' @return date
#' @export
decimal_to_date <- function(dec_date, ...) {
  dec_date <- as.vector(dec_date)
  return(lubridate::date_decimal(dec_date))
}


#' Tidy time series in data frame
#'
#' @param y observations
#' @param type type of observations
#' @param ... not used
#'
#' @return tidy ts
#' @export
tidier_ts <- function(y, type = 'actual', ...) {
  dec_date <- decimal_to_date(time(y))
  res <- dplyr::data_frame(date = as.Date(dec_date), type = type, value = as.vector(y))
  return(res)
}


#' Time sequence
#' 
#' To be improved or removed in the future.
#'
#' @param y observations
#' @param start_pos where to start
#' @param out_length how many to generate
#'
#' @return time sequence
#' @export
time_sequence <- function(y, start_pos = 'max', out_length) {
  # get start date
  dec_date <- match.fun(start_pos)(time(y))
  # get start date
  start_date <- decimal_to_date(dec_date)
  # define increment
  increment <- date_increment(y = y)
  # generate sequence
  date <- seq.Date(from = as.Date(start_date), length.out = out_length, by = increment)
  # return results
  return(date)
}

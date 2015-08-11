#' Function to generate dates
#' 
#' @description This function generates random dates between the start date and end date.
#' 
#' @author Rui Zheng
#' 
#' @param date.start The start date, in the format of "year-month-day"
#' @param date.end The end date, in the format of "year-month-day"
#' @param size The number of dates you want to generare.
#' 
#' @example get_random_dates('1929-02-20', '1929-03-30', 414)
#' 
#' @export


get_random_dates <- function(date.start, date.end, size) {
  start <- as.Date(date.start)
  end <- (as.Date(date.end))
  # get the number of days between start and end
  days <- as.numeric(difftime(end, start, unit='day'))
  random_days <- runif(size, 0, days) 
  random_dates <- start + random_days
  return(random_dates)
}

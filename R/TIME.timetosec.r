#' mm:ss to number of seconds
#' 
#' @description This function converts the normal expression of time
#' (mm:ss) to the number of seconds. It can save a few 
#' line of codes during walking speed calculation
#' 
#' @return A number that shows the number of seconds
#' 
#' @param var Format: mm:ss
#' 
#' @examples
#' timetosec(sppb$time)
#' 
#' @export
timetosec <- function (var) {
  timeinsec<-sapply(strsplit (var, ":"),
                    function(x) {
                      x <- as.numeric(x)
                      x[1]*60+x[2]
                    }
  )
  return(timeinsec)
}
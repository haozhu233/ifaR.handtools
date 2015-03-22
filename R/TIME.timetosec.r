#' mm:ss to number of seconds
#' 
#' @description This function converts the normal expression of time
#' (HH:mm:ss, mm:ss, ss) to the number of seconds. It can save a few 
#' line of codes during walking speed calculation
#' 
#' @return A number that shows the number of seconds
#' 
#' @param var Format: HH:mm:ss, mm:ss, ss
#' 
#' @examples
#' timetosec(sppb$time)
#' 
#' @export
timetosec <- function(x){
  if (!is.character(x)) stop("x must be a character string of the form H:M:S")
  if (length(x)<=0)return(x)
  
  unlist(
    lapply(x,
           function(i){
             i <- as.numeric(strsplit(i,':',fixed=TRUE)[[1]])
             if (length(i) == 3) 
               i[1]*3600 + i[2]*60 + i[3]
             else if (length(i) == 2) 
               i[1]*60 + i[2]
             else if (length(i) == 1) 
               i[1]
           }  
    )  
  )  
} 
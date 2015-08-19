#' Pround
#' @author TGT
#' @export
# rounds p-values to a number of digits which depends on the value of p

pround<-function(p){
  r <- NULL
# two digits for p >= 0.06
  if (p >= 0.06) r<-2
  
# 3 digits for 0.001 <= p < 0.06  
  if (p >= 0.001 & p < 0.06) r <- 3
  if(!is.null(r)) out<-formatC(p, digits=r, format="f", drop0trailing=F)
  if (p > 0.99) out <-  "> 0.99"
  if (p < 0.001) out <- "< 0.001"
  out
}
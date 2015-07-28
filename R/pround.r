#' Pround
#' @author TGT
#' @export

pround<-function(p){
  r <- NULL
  if (p >= 0.06) r<-2
  if (p >= 0.001 & p < 0.06) r <- 3
  if(!is.null(r)) out<-formatC(p, digits=r, format="f", drop0trailing=F)
  if (p > 0.99) out <-  "> 0.99"
  if (p < 0.001) out <- "< 0.001"
  out
}
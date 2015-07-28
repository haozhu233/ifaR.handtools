#' Get Fisher
#' @author TGT
#' @export

getfisher<-function(aecount){
  fisher.test(matrix(c(aecount, c(75, 74)-aecount), nrow=2, byrow=T))$p.value
}
#' Function to compute 0.95 confidence interval for the difference in two means
#' 
#' @author TGT
#' @note g is grouping variable
#' @export
bootdif <- function(y, g) {
  g <- as.factor(g)
  a <- attr(smean.cl.boot(y[g==levels(g)[1]], B=1000, reps=TRUE),'reps')
  b <- attr(smean.cl.boot(y[g==levels(g)[2]], B=1000, reps=TRUE),'reps')
  meandif <- diff(tapply(y, g, mean, na.rm=TRUE))
  a.b <- quantile(b-a, c(.025,.975))
  res <- c(meandif, a.b)
  names(res) <- c('Mean Difference','.025','.975')
  res
}
#' Function to compute 0.95 confidence interval for the difference in two means
#' 
#' @author TGT
#' @note g is grouping variable
#' @export


# in order to make this work, need to install the Hmisc library(Add Warning?)
# smean.cl.boot: fast implementation of the basic nonparametric bootstrap
#                for obtaining confidence limis for the population mean
#                without assumming normality.


bootdif <- function(y, g) {
  g <- as.factor(g)
  
  # vector of means from 1000 times bootstrapped resampling for each group
  a <- attr(smean.cl.boot(y[g==levels(g)[1]], B=1000, reps=TRUE),'reps')
  b <- attr(smean.cl.boot(y[g==levels(g)[2]], B=1000, reps=TRUE),'reps')
  
  # get difference of the two groups' means
  meandif <- diff(tapply(y, g, mean, na.rm=TRUE))
  
  # get the 95 % confidence limits from bootstrapped resampling
  a.b <- quantile(b-a, c(.025,.975))
  
  # result is a vector with value of difference of means and upper and lower limits
  res <- c(meandif, a.b)
  names(res) <- c('Mean Difference','.025','.975')
  res
}

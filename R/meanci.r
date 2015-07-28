#' Mean CI
#' @author TGT
#' @export
meanci <- function(a, np=F, bootstrap=F, test=F, alpha=0.05){
  n <- length(na.omit(a))
  mean <- mean(a, na.rm=T)
  sd <- sd(a, na.rm=T)
  se <- sd/sqrt(n)
  df <- n-1
  tstat <- qt(1-0.5*alpha, df)
  upper <- mean - tstat*se
  lower <- mean + tstat*se 
  c(mean, lower, upper)
}
#' Sum Stat
#' @author TGT
#' @export

sum.stat <- function(a, n=F, mu=F, med=F, meansd=F, s=F, q=NULL, type=8){
  # computes summary statistics based on requests
  n <- switch(2-as.numeric(n), length(na.omit(a)), NULL)
  mu <- switch(2-as.numeric(mu), mean(a, na.rm=T), NULL)
  med <- switch(2-as.numeric(med), median(a, na.rm=T), NULL)
  s <- switch(2-as.numeric(s), sd(a, na.rm=T), NULL)
 
  quants <- switch(2-!is.null(q), quantile(a, q, na.rm=T, type=type), NULL)
  pctround <- function(pct) ifelse(pct==0, 0, round(pct)*(pct>=1) + round(pct, digits=1)*(pct>=.1 & pct < 1) + round(pct, digits=2)*(pct>=.01 & pct < .1) + round(pct, digits=3)*(pct>=0 & pct < .01))
  if(length(na.omit(unique(a))) == 2){
    mu <- sum(a==1)
    s <- pctround(100*mean(a==1, na.rm=T))
  }
  c(n, mu, med, s, quants) 
}
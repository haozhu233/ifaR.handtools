#' Summary Statistics
#' 
#' @description This function works together with data.table to generate summary 
#' statistics information.
#' 
#' @param a A Variable
#' @param n Whether the number of non-missing/null values will be displayed
#' @param mu
#' 
#' @export
sum.stat<-function(a, n=F, mu=F, s=F, q=NULL, type=8){
  # computes summary statistics based on requests
  n<-switch(2-as.numeric(n), length(na.omit(a)), NULL)
  if(!is.null(n)) names(n)<-"N"
  mu<-switch(2-as.numeric(mu), mean(a, na.rm=T), NULL)
  if(!is.null(mu)) names(mu)<-"Mean"
  s<-switch(2-as.numeric(s), sd(a, na.rm=T), NULL)
  if(!is.null(s)) names(s)<-"SD"
  quants<-switch(2-!is.null(q), quantile(a, q, na.rm=T, type=type), NULL)
  if(!is.null(quants)) names(quants) <- paste("P", 100*q, sep="")
  c(n, mu, s, quants)
}

library(data.table)
dt<-data.table(grp=rep(c("Control", "Treatment"), each=50),
               time=rep(c("Base", "Follow", "Base", "Follow"), each=25),
               age=rnorm(100, mean=65, sd=10),bmi=rnorm(100, mean=28, sd=4),
               leanmass=rnorm(100, mean=65, sd=6))
dt$leanmass[95]<-NA
dt[, as.list(unlist(lapply(.SD, sum.stat, n=F, mu=T, s=T, q=c(.25, .5, .75)))), by=.(grp, time)]

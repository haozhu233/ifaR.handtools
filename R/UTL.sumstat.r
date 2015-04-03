#' Summary Statistics
#' 
#' @description This function works together with data.table to generate summary 
#' statistics information.
#' 
#' @param a A Variable
#' @param n Whether the number of non-missing/null values will be displayed
#' @param mu Whether the value of mean will be calculated
#' @param s Whether the value of standard deviation will be calculated
#' @param q A list of quantiles
#' @param type Quantile type
#' @param round.N Round digits
#' 
#' @examples
#' library(data.table)
#' library(ifaR.handtool)
#' dt<-data.table(grp=rep(c("Control", "Treatment"), each=50),
#'                time=rep(c("Base", "Follow", "Base", "Follow"), each=25),
#'                age=rnorm(100, mean=65, sd=10),bmi=rnorm(100, mean=28, sd=4),
#'                leanmass=rnorm(100, mean=65, sd=6))
#' dt$leanmass[95]<-NA
#' dt[, as.list(unlist(lapply(.SD, sumstat, n=F, mu=T, s=T, q=c(.25, .5, .75)))), by=.(grp, time)]
#' 
#' @export
sumstat<-function(a, n=F, mu=F, s=F, q=NULL, q.type=8, mdn=F, mnm=F, sem=F,round.N=3){
  # computes summary statistics based on requests
  if (all(na.omit(a) %in% 0:1) & sum(is.na(a))!=length(a)) {
    number<-switch(2-as.numeric(mu), round(length(na.omit(a))*mean(a, na.rm=T),0), NULL)
    if(!is.null(number)) names(number)<-"n"
    percentage<-switch(2-as.numeric(s), round(mean(a, na.rm=T), round.N), NULL)
    if(!is.null(percentage)) names(percentage)<-"p"
    c(number, percentage)
  }
  else {
    n<-switch(2-as.numeric(n), length(na.omit(a)), NULL)
    if(!is.null(n)) names(n)<-"N"
    mu<-switch(2-as.numeric(mu), round(mean(a, na.rm=T), round.N), NULL)
    if(!is.null(mu)) names(mu)<-"Mean"
    s<-switch(2-as.numeric(s), round(sd(a, na.rm=T), round.N), NULL)
    if(!is.null(s)) names(s)<-"SD"
    quants<-switch(2-!is.null(q), round(quantile(a, q, na.rm=T, q.type=q.type), round.N), NULL)
    if(!is.null(quants)) names(quants) <- paste("P", 100*q, sep="")
    mdn<-switch(2-as.numeric(mdn), round(median(a, na.rm=T), round.N), NULL)
    if(!is.null(mdn)) names(mdn)<-"Median"
    mnm<-switch(2-as.numeric(mnm), round(abs(mean(a, na.rm=T)-median(a, na.rm=T)), round.N), NULL)
    if(!is.null(mnm)) names(mnm)<-"|Mean-Median|"
    sem<-switch(2-as.numeric(sem), round(sd(a, na.rm=T)/sqrt(length(na.omit(a))), round.N), NULL)
    if(!is.null(sem)) names(sem)<-"SD"
    c(n, mu, s, quants, mdn, mnm, sem)
  }
}
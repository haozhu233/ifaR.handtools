#' Table 1
#' @author TGT
#' @export

table1<-function(dt, by=NULL, meansd=T, n=F, mu=F, s=F, q=NULL, type=8, sdparen=T, rnd=0){ # subgroup
  require(reshape2)
  require(data.table)
  ndigits <- function(a){
    out <- max(rnd + (abs(a)<20) + (abs(a)<1) + (abs(a)<.1), 0)
    if(sum(a[c(T,F)] == floor(a[c(T,F)]))) out<-0
    out
  }
  form <- function(a) {
    a[!is.na(a)]<-formatC(a[!is.na(a)], digits=max(ndigits(a[!is.na(a)]), na.rm=T), format="f", drop0trailing=F)
    a
  }
  if (meansd){
    mu<-T
    s<-T
    n<-F
    q<-NULL
  }
  dt<-data.table(dt, key=by)
  # setkey(dt, by)
  kount <- dt[, .N, by=key(dt)]
  dt<-dt[, lapply(.SD, sum.stat, n=n, mu=mu, s=s, q=q, type=type), by=by] # subgroup
  datacols<-(length(by)+1):ncol(dt)
  #datanames <- names(dt)[datacols]
  dt[, stat := c("N", "Mean", "SD", paste("P", 100*q, sep=""))[which(c(n, mu, s, !is.null(q)*q))]]
  dt[, (datacols) := lapply(.SD, form), .SDcols=datacols]
  rstring <- function(a) as.character(round(as.numeric(a)))
  enclose <- function(a) paste("(", a, ")", sep="")
  dt[stat=="N", (datacols) := lapply(.SD, rstring), .SDcols=datacols]
  if(sdparen) dt[stat=="SD", (datacols) := lapply(.SD, enclose), .SDcols=datacols]
  setcolorder(dt, c(by, "stat", names(dt)[datacols]))
  if(meansd){
    dt <- melt(dt, 1:(length(by)+1), (length(by)+2):ncol(dt))
    dt <- dcast.data.table(dt, ... ~ stat)
    dt[, meansd := paste(Mean, SD)]
    dt[, c("Mean", "SD") := NULL]
    dt<-dcast.data.table(dt, ... ~ variable, value.var="meansd")
    addkount<-function(a) paste("\\thead{", a, " \\\\ (N = ", kount$N, ")}", sep="")
    dt[, (length(by)) := lapply(.SD, addkount), .SDcols=length(by)]
  }
  dt
}
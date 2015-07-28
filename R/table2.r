#' Table2
#' @author TGT
#' @export


table2<-function(thedata, by, exclude, seed=NULL, rnd=0){ # subgroup
  require(reshape2)
  require(data.table)
  thedata<-as.data.frame(thedata)
  thedata<-thedata[, !(names(thedata) %in% exclude)]
  # require(data.table)
  ndigits <- function(a){
    out <- max(rnd + (abs(a)<20) + (abs(a)<1) + (abs(a)<.1), 0)
    if(sum(a[c(T,F)] == floor(a[c(T,F)]))) out<-0
    out
  }
  form <- function(a) {
    a[!is.na(a)]<-formatC(a[!is.na(a)], digits=max(ndigits(a[!is.na(a)]), na.rm=T), format="f", drop0trailing=F)
    a
  }
  out<-apply(thedata[,names(thedata)!=by], 2, bootci.mean, by=thedata[, by], seed=seed)
  out<-t(apply(out, 2, form))
  mkci<-function(b) c(paste(b[1], " (", b[2], ", ", b[3], ")", sep=""),
                      paste(b[4], " (", b[5], ", ", b[6], ")", sep=""),
                      paste(b[7], " (", b[8], ", ", b[9], ")", sep=""))
  out<-as.data.frame((t(apply(out, 1, mkci))))
  names(out) <- c(levels(as.factor(thedata[, by])), "Difference")
  noquote(out)
}
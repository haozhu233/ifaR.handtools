#' Two to one (Table re-formating)
#' 
#' @description This function combines two elements into one in a format as "Mean (SD)".
#' It converts the outputs of sumstat to a simplified format, which is ready for publishing 
#' through markdown. 
#' 
#' @param a The input should be a summary table, whose columns are the stratifying groups 
#' and rows are pairs of variablew in the format of "variable.Mean", "variable.SD"...etc.  
#' 
#' @examples
#' library(data.table)
#' library(ifaR.handtool)
#' dt<-data.table(grp=rep(c("Control", "Treatment"), each=50),
#'                time=rep(c("Base", "Follow", "Base", "Follow"), each=25),
#'                age=rnorm(100, mean=65, sd=10),bmi=rnorm(100, mean=28, sd=4),
#'                leanmass=rnorm(100, mean=65, sd=6))
#' dt$leanmass[95]<-NA
#' 
#' tab1<-sumtbl(dt, c("age", "bmi", "leanmass"), c("grp", "time"), n=F, mu=T, s=T, q=NULL)
#' twotoone(tab1)
#' 
#' @export 
twotoone<-function(a){
  a.rownames<-rownames(a)
  a.rowN<-nrow(a)
  if(a.rowN %% 2 != 0) {stop("You have to have even number of rows to use this function.")}
  new.colnames<-colnames(a)
  new.colN<-ncol(a)
  new.rownames<-unique(sub('[.][^.]+$', '', a.rownames))
  new.rowN<-a.rowN/2
  new.table<-data.frame(matrix(NA,nrow=new.rowN, ncol=new.colN))
  colnames(new.table)<-new.colnames
  rownames(new.table)<-new.rownames
  for (i in 1:new.colN){
    for (j in 1:new.rowN){
      new.table[j,i]<-paste(a[j*2-1,i], " (",a[j*2,i], ")", sep="")
    }
  }
  return(new.table)
}
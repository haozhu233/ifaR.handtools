#' Find out NAs and outliers in a variable(column)
#'
#' @description This function will find out missing values and outliers in a 
#' certain variable in a dataset. Outliers are defined as mean +- \code{iqrband} 
#' * interquartile range (IQR). Values exceed the \code{min} and \code{max} will 
#' also be marked as outliers. 
#'
#' @return This function will return a new dataset with the first column as 
#' subject/study ID and the second column as the outlying status of the input
#' variable. Missing values will be displayed as NA. The actual values for those
#' outliers will be displayed. 
#' 
#' @usage outvalues(dataset, variable, iqrband=2, min=NULL, max=NULL, type=8)
#' 
#' @param dataset Name of the dataset
#' @param variable Name of the variable.It should be entered as a character. For
#' example, "BMI". 
#' @param iqrband The multiplier of IQR. Pick 3 for the very end extreme values.
#' Pick 2 to be more inclusive
#' @param min The smallest value that will not be determined as outliers
#' @param max The largest value that will not be determined as outliers
#' @param type An integer between 1 and 9 selecting one of the nine quantile 
#' algorithms detailed below to be used.
#' 
#' @examples
#' outvalues(baseline, "BMI") # default iqrband=2
#' outvalues(baseline, "BMI", iqrband=3, min=15, max=35, type=8)
#'
#' @export

ifar.extre.marker<-function(data, variable){
  var<-data[, variable]
  data<-data[order(var),]
  var<-data[, variable]
  var.marker<-paste(variable, "MARKER", sep=".")
  data[c(1:5, as.numeric(length(var) - sum(is.na(var)) - 4):as.numeric(length(var) - sum(is.na(var)))), var.marker]<-"X"
  data[is.na(data[, var.marker]), var.marker]<-""
  return(data)
}
outvalues <- function(dataset, variable, iqrband=2, min=NULL, max=NULL, type=8){
  
  # a is a vector for which we want to identify missing or outlying values
  # iqrband is the number of lower and upper half interquartile intervals
  # considered not outlying
  # min, max are plausible lower and upper limits of a for the relevant population
  # and/or technical limiting quantities e.g. assay limit of detection
  # type is quantile type [see ?quantile()]
  a<-dataset[,c(variable)]
  iqi<-quantile(a, probs=c(.25, .5, .75), na.rm=T, type=type)
  
  upper<-iqi[2]+iqrband*(iqi[3]-iqi[2]) # apply iqband separately to lower and upper
  lower<-iqi[2]-iqrband*(iqi[2]-iqi[1]) # fences, to accommodate skew 
  
  if(!is.null(min)) lower <- max(lower, min) # choose limits more inclusive of outlier class
  if(!is.null(max)) upper <- min(upper, max) 
  
  out.order.NA<-which(is.na(a))
  out.order.high<-which(a>upper)
  out.order.low<-which(a<lower)
  
  new.col.name<-paste(variable, ".OUT",sep="")
  dataset[,c(new.col.name)]<-NA
  dataset[out.order.NA, c(new.col.name)]<-"NA"
  dataset[out.order.high, c(new.col.name)]<-a[out.order.high]
  dataset[out.order.low, c(new.col.name)]<-a[out.order.low]
  return(dataset[,c(1,ncol(dataset))])
}
#' Find out extre values
#'
#' @description This function will find out the 5 highest and 5 lowest values and put an
#' "X" after their ID. 
#'
#' @return This function will return a new dataset with a new column which is named as 
#' "variable.marker" and marks out the 5 highest and 5 lowest values. 
#' 
#' @usage ifar.extre.marker(dataset, variable)
#' 
#' @param dataset Name of the dataset
#' @param variable Name of the variable.It should be entered as a character. For
#' example, "BMI". 
#' 
#' @examples
#' ifar.extre.marker(baseline, "BMI")
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
#' Boot CI Mean
#' @author TGT
#' @export

bootci.mean<-function(a, by, B=1000, level=0.95, delta=T, seed=NULL){
  if(!is.null(seed)) set.seed(seed)
  by <- as.factor(by)
  a1 <- as.numeric(na.omit(a[by == levels(by)[1]]))
  a2 <- as.numeric(na.omit(a[by == levels(by)[2]]))
  ma1 <- mean(a1)
  ma2 <- mean(a2)
  mdelta <- ma2-ma1
  n1 <- length(a1)
  n2 <- length(a2)
  bootsample.a1 <- matrix(sample(a1, n1*B, replace=T), nrow=n1)
  bootsample.a2 <- matrix(sample(a2, n2*B, replace=T), nrow=n2)
  means.a1 <- apply(bootsample.a1, 2, mean)
  means.a2 <- apply(bootsample.a2, 2, mean)
  means.d <- means.a2 - means.a1
  ci<-function(x) quantile(x, c(0.025, 0.975))
  # meanci<-function(x) c(mean(x), quantile(x, c(.025, .975)))
  out<-c(ma1, ci(means.a1), ma2, ci(means.a2), mdelta, ci(means.d))
  names(out)<-paste(c(rep(levels(by), each=3), rep("Delta", 3)), rep(c(".Mean", ".Lower", ".Upper"), 3), sep="")
  out
  #vars.a1 <- apply(bootsample.a1, 2, var)
  #vars.a2 <- apply(bootsample.a2, 2, var)
  #ses.a1 <- sqrt(vars.a1/n1)
  #ses.a2 <- sqrt(vars.a2/n2)
  #ses.d <- sqrt(vars.a1/n1 + vars.a2/n2)
}
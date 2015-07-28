#' Get Linear and Nonlinear Mixed Effects Models
#' @author TGT
#' @export

getlme<-function(y, data, rnd=0){
  require(nlme)
  ndigits <- function(a, rnd=0){
    out <- max(rnd + (abs(a)<20) + (abs(a)<1) + (abs(a)<.1), 0)
    if(sum(a[c(T,F)] == floor(a[c(T,F)]))) out<-0
    out
  }
  form <- function(a) {
    a[!is.na(a)]<-formatC(a[!is.na(a)], digits=max(ndigits(a[!is.na(a)], rnd=rnd), na.rm=T), format="f", drop0trailing=F)
    a
  }
  pround<-function(p){
    r <- NULL
    if (p >= 0.06) r<-2
    if (p >= 0.001 & p < 0.06) r <- 3
    if(!is.null(r)) out<-formatC(p, digits=r, format="f", drop0trailing=F)
    if (p > 0.99) out <-  "> 0.99"
    if (p < 0.001) out <- "< 0.001"
    out
  }
  data1 <- as.data.frame(eval(as.name(paste(data, "delta", sep="."))))
  data2 <- as.data.frame(eval(as.name(paste(data, "delta", "pp", sep="."))))
  y1<- data1[, paste(y, "delta6", sep=".")]
  y2<- data2[, paste(y, "delta6", sep=".")]
  sex1 <- data1[, "SEX"]
  sex2 <- data2[, "SEX"]
  x1 <- data1[, "GROUP"]
  x2 <- data2[, "GROUP"]
  base1 <- data1[, paste(y, "base", sep=".")]
  base2 <- data2[, paste(y, "base", sep=".")]
  site1 <- data1[, "SITE"]
  site2 <- data2[, "SITE"]
  lme1 <- lme(y1 ~ x1 + base1 + sex1, random = ~ 1 | site1, na.action=na.omit)
  lme2 <- lme(y2 ~ x2 + base2 + sex2, random = ~ 1 | site2, na.action=na.omit)
  c1 <- summary(lme1)$tTable
  c2 <- summary(lme2)$tTable
  t1 <- qt(.975, c1[2,3])
  t2 <- qt(.975, c2[2,3])
  s1 <- c1[2,2]
  s2 <- c2[2,2]
  p1 <- c1[2, 5]
  p2 <- c2[2, 5]
  b1 <- c1[2, 1]
  b2 <- c2[2, 1]
  upper1 <- b1 + t1*s1
  lower1 <- b1 - t1*s1
  upper2 <- b2 + t2*s2
  lower2 <- b2 - t2*s2
  row1 <- form(c(b1, lower1, upper1))
  row2 <- form(c(b2, lower2, upper2))
  row1 <- c(row1[1], paste("(", row1[2], ", ", row1[3], ")", sep=""))
  row2 <- c(row2[1], paste("(", row2[2], ", ", row2[3], ")", sep=""))
  c(y, row1, pround(p1), row2, pround(p2))
}
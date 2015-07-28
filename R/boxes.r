#' Fancy box plots
#' @author TGT
#' @describeIn This function can produce box plots with detailed information
#'
#' @export
#'
boxes<-function(y, by, col = c(rgb(0,0,0,.2), rgb(0,0,1, .2)), border = c("black", "blue"), medcolor = c("black", "blue"), xlab=""){
  x <- by
  n <- length(unique(x))
  lims <- c(min(y, na.rm=T), max(y, na.rm=T))
  width<-rep(.25, n)
  plot(n, n, xlim=lims, ylim=c(0.5, n+.5), ann=F, axes=F, type="n")
  b1 <- boxplot(y~x, plot=F)
  med <- b1$stats[3,]
  uhinge <-b1$stats[4,]
  lhinge <- b1$stats[2,]
  lwhisker <- b1$stats[1, ]
  uwhisker <- b1$stats[5, ]

  for(i in 1:n){
    polygon(rep(c(lhinge[i], uhinge[i]), each=2), c(i+width[i], i-width[i], i-width[i], i+width[i]), col=col[i], border=border[i])
    # segments(med[i], i-width[i], med[i], i+width[i], col=medcolor[i])
    segments(med[i], i-width[i], med[i], i+width[i], col=border[i])
    segments(uhinge[i], i, uwhisker[i], i, col=border[i])
    segments(uhinge[i], i, uwhisker[i], i, col=border[i])
    segments(uwhisker[i], i-width[i]/4, uwhisker[i], i+width[i]/4, col=border[i])
    segments(lhinge[i], i, lwhisker[i], i, col=border[i])
    segments(lhinge[i], i, lwhisker[i], i, col=border[i])
    segments(lwhisker[i], i-width[i]/4, lwhisker[i], i+width[i]/4, col=border[i])
    points(b1$out[b1$group==i], rep(i, length(b1$out[b1$group==i])), pch=16, col=border[i], cex=.5)
    pts<-y[x==b1$names[i] & y >= lwhisker[i] & y <= uwhisker[i]]
    pts.y <- rep(i+.15, length(pts))
    points(pts, pts.y, pch="|", cex=.3)
  }

  par(mgp=c(3,1,0))
  axis(side=2, at = 1:n, labels=b1$names, las=1, tick=F, cex.axis=.65, xpd=NA)
  par(mgp=c(4, 0, -.15))
  axis(side=1, tcl=.1, cex.axis=.55)
  mtext(side=1, xlab, cex=.65, line=1, xpd=NA)
}

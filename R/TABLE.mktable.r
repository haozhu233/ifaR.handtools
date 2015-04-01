#' Make Table
#' 
#' @description Convert R data frame object x to a nice pdf table via latex
#' 
#' @param x A dataframe
#' @param placement not sure what that is...
#' @param caption The table caption
#' @param label The table label
#' @param include.rownames Whether or not the rownames should be included
#' 
#' @seealso xtable
#' @examples
#' mktable(baseline, caption="Table 1.", label="")
#' 
#' @export
mktable<-function(x, placement="ht", caption="", label="", footnotes=NULL, headerlabel="", size="small", footnotesize="scriptsize", labelwidth = 0.1, width=1, alignment="", use.rownames=F, use.colnames=F, shade=F, header="", headercols=0, labelrows=1, rnd=NULL){
  
  shading <- cmid <- head <- footitems <- tabnotes <- ""
  if(shade) shading <- "\\rowcolor[rgb]{.96, .98, 1}"
  
  tableheaders <- noquote(t(as.matrix(x[, 1:headercols, with=FALSE])))
  tabledata<-t(x[,(headercols+1):ncol(x), with=FALSE])
  
  # look for columns that contain only integer variables
  ints<-rep(NA, nrow(tabledata))
  for(i in 1:ncol(tabledata)) {ints[i] <- identical(sum(100*round(tabledata[,i]/100, 2)), sum(tabledata[,i]))}
  
  # sensible rounding
  ndigits<-function(a) (a<20)+(a<1)+(a<.1)
  # significant digits for rounding, constant for each table row (matrix column)
  if(!is.null(rnd)) rnd<-rep(rnd, nrow(tabledata))
  if(is.null(rnd)) rnd<-apply(ndigits(tabledata), 1, max)
  
  # set up formatting, keep trailing 0s
  tabledata2<-tabledata
  substrRight <- function(x, n){
    substr(x, nchar(x)-n+1, nchar(x))
  }
  for(i in 1:nrow(tabledata2)){
    tabledata[i,] <- formatC(tabledata2[i,], format="f", digits=rnd[i])
    tabledata[,ints] <- tabledata2[,ints]
  }
  
  x<-noquote(rbind(tableheaders, tabledata))
  
  x<-noquote(cbind(row.names(x), x))
  x[1:headercols, 1] <- ""
  x[max(1, headercols), 1] <- headerlabel
  
  # top matter preceding header; use threeparttable package
  preamble<-paste("\\begin{table}[", placement, "] \\begin{threeparttable}\n\\caption{", caption, "}\n\\label{", label, "}\n{\\", size, "\n\\begin{tabulary}{", width, "\\textwidth}{@{}p{",
                  labelwidth, "\\textwidth}", alignment, "@{}}\n \\toprule\n", sep="")
  
  ### set up header
  
  chg<-function(a) (a!=c("", a))[-(length(a)+1)]
  
  # function to create latex coding of a single header row
  headerrow<-function(a){
    heads<-a[chg(a)]
    srt<-which(chg(a))
    stp<-c(srt[-1]-1, length(a))
    spans<-stp-srt+1
    first<-srt[1]
    pre<-""
    if (first>1) pre<-paste(rep("& ", first-1), collapse="")
    paste(pre, paste("\\multicolumn{", spans, "}{C}{", heads, "}", sep="", collapse =" & "), "\\\\ \n", sep="")
  }
  
  # construct each header row, concatenate all but lowest into single string "head"; this will be separated from the lowest header row by cmidrules
  
  # set up header rows
  for (i in 1:headercols){
    assign(paste("headrow.", i, sep=""), headerrow(x[i,]))
  }
  head<-headrow.1
  if (headercols>2){
    for(i in 2:(headercols-1)){head<-paste(head, eval(as.name(paste("headrow.", i, sep=""))))}
  }
  
  if (headercols>1){
    a <- x[headercols-1,]
    srt<-which(chg(a))
    stp<-c(srt[-1]-1, length(a))
    cmid<-paste("\\cmidrule(lr){", paste(srt, stp, sep="-"), "} ", collapse="", sep="")
  }
  
  # assemble the header
  header<-paste(head, cmid, sep="")
  if(headercols > 1) header <- paste(header, eval(as.name(paste("headrow.", headercols, sep=""))), sep="") # attach last header row
  header <- paste(header, "\\midrule\n", sep="")
  
  
  # assemble end matter (coda) including footnotes
  if(!is.null(footnotes)){
    footmarks<-c("a", "b", "c", "d", "e", "f", "g", "h", "i", "j", "k", "l", "m", "n", "o", "p", "q", "r", "s", "t", "u", "v", "w", "x", "y", "z")[1:length(footnotes)]
    footitems<-paste("\\item[", footmarks, "]", footnotes, " \n ", sep="", collapse="")
    tabnotes<-paste("\n\\begin{tablenotes} \n \\", footnotesize, footitems, " \\end{tablenotes} \n ", sep="")
  }
  coda<-paste("\\bottomrule\n\\end{tabulary}\n}", tabnotes, "\\end{threeparttable} \n \\end{table}\n", sep="")
  
  # assemble body
  
  bodydata<-x[(headercols+1):nrow(x),]
  tablerow<-function(d) paste(paste(d, collapse=" & "), " \\\\ \n ", sep="")
  data<-apply(bodydata,1,tablerow)
  data[seq(2, length(data), 2)]<-paste(shading, "[0.5pt]",  data[seq(2, length(data), 2)], sep="")
  body<-paste(data, collapse="")
  
  # output table
  cat(paste(preamble, header, body, coda, sep="", collapse=""))
  
}
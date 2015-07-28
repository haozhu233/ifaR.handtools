#' Render multi-level table with multiple customization options enabled
#' @author TGT
#' @description This function provides an easy way to render a well-formated LATEX table.
#'
#' @export

mktable2<-function(x, transpose=F, placement="ht", caption="", shortcap=caption, label="", row.descrip=NULL, colnames=NULL, footnotes=NULL, headerlabel=NULL, size="small", footnotesize="scriptsize", labelwidth = 0.1, width=1, alignment="", use.rownames=T, use.colnames=F, shade=F, header="", headerrows=0, rnd=NULL){
  if(headerrows ==0 ) {
    use.colnames <- T
    headerrows <- 1}

  shading <- cmid <- head <- footitems <- tabnotes <- ""
  if(shade) shading <- "\\rowcolor[rgb]{.96, .98, 1}"

  if(transpose) x<-(t(as.matrix(x)))

  # x <- as.data.frame(x)
  # i <- sapply(x, is.factor)
  # x[i] <- lapply(x[i], as.character)

  if(use.rownames) x <- cbind(row.names(x), x)
  x <- as.data.frame(x)
  i <- sapply(x, is.factor)
  x[i] <- lapply(x[i], as.character)
  if(use.colnames) x <- rbind(names(x), x)

  if(!is.null(row.descrip)) x[(headerrows+1):nrow(x), 1] <- row.descrip
  if(!is.null(colnames)) x[1, (ncol(x)-length(colnames)+1):ncol(x)]<-colnames
  if(headerrows>1) x[1:headerrows, 1] <- ""
  x<-noquote(x)

  preamble<-paste("\\begin{table}[", placement, "] \\begin{threeparttable}\n\\caption[", shortcap, "]{", caption, "}\n\\label{", label, "}\n{\\", size, "\n\\begin{tabulary}{", width, "\\textwidth}{@{}p{",labelwidth, "\\textwidth}", alignment, "@{}}\n \\toprule\n", sep="")

  ### set up header
  ###### here.s the issue

  chg <- function(a) as.logical(c(1, diff(as.numeric(factor(as.character(a))))!=0))  # function(a) (a!=c("", a))[-(length(a)+1)]
  nc<-ncol(x)

  # function to create latex coding of a single header row
  headerrow<-function(a){
    a <- as.character(a)
    heads<-a[chg(a)]
    srt<-which(chg(a))
    stp<-c(srt[-1]-1, length(a))
    spans<-stp-srt+1
    first<-srt[1]
    pre<- ""
    if (first>1) pre<-paste("\\multicolumn{1}{l}{}", rep("& ", first-1), collapse="")
    paste(pre, paste("\\multicolumn{", spans, "}{", rep("c", length(heads)), "}{", heads, "}", sep="", collapse =" & "), "\\\\ \n", sep="")
  }

  # construct each header row, concatenate all but lowest into single string "head"; this will be separated from the lowest header row by cmidrules

  # set up header rows

  if(use.rownames) x[, 1] <- as.character(x[, 1]) # just uncommented
  if(use.rownames) x[max(1, headerrows), 1] <- ""
  if(!is.null(headerlabel)) x[max(1, headerrows), 1] <- headerlabel
  if(headerrows == 1){
    head<-paste(paste("\\multicolumn{", 1, "}{" , c("l", rep("c", nc-1)), "}{", x[1,], "}", sep="", collapse =" & "), "\\\\ \n",
                sep="")
  }

  if(headerrows > 1){
    for (i in 1:headerrows){
      assign(paste("headrow.", i, sep=""), headerrow(x[i,]))
    }
    head<-headrow.1}
  if (headerrows>2){
    for(i in 2:(headerrows-1)){head<-paste(head, eval(as.name(paste("headrow.", i, sep=""))))}
  }
  if (headerrows>1){
    a <- x[headerrows-1,]
    # don't put cmids over column with empty header (i.e. row.names column)
    srt<-which(chg(a))
    stp<-c(srt[-1]-1, length(a))
    if(a[1]=="") {
      # don't put cmids over column with empty header (i.e. row.names column)
      srt<-srt[-1]
      stp<-stp[-1]
    }
    cmid<-paste("\\cmidrule(lr){", paste(srt, stp, sep="-"), "} ", collapse="", sep="")
  }

  # assemble the header
  header<-paste(head, cmid, sep="")
  if(headerrows > 1) header <- paste(header, eval(as.name(paste("headrow.", headerrows, sep=""))), sep="") # attach last header row
  header <- paste(header, "\\midrule\n", sep="")

  # assemble end matter (coda) including footnotes
  if(!is.null(footnotes)){
    footmarks<-c("a", "b", "c", "d", "e", "f", "g", "h", "i", "j", "k", "l", "m", "n", "o", "p", "q", "r", "s", "t", "u", "v", "w", "x", "y", "z")[1:length(footnotes)]
    footitems<-paste("\\item[", footmarks, "]", footnotes, " \n ", sep="", collapse="")
    tabnotes<-paste("\n\\begin{tablenotes} \n \\", footnotesize, footitems, " \\end{tablenotes} \n ", sep="")
  }
  coda<-paste("\\bottomrule\n\\end{tabulary}\n}", tabnotes, "\\end{threeparttable} \n \\end{table}\n", sep="")

  # assemble body

  bodydata<-x[(headerrows+1):nrow(x),]
  tablerow<-function(d) paste(paste(d, collapse=" & "), " \\\\ \n ", sep="")
  data<-apply(bodydata,1,tablerow)
  #data[seq(2, length(data), 2)]<-paste(shading, "[0.5pt]",  data[seq(2, length(data), 2)], sep="")
  body<-paste(data, collapse="")

  # output table
  cat(paste(preamble, header, body, coda, sep="", collapse=""))
}

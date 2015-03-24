#' Make Table
#' 
#' @description Convert R data frame object x to a nice pdf table via latex
#' 
#' @param x A dataframe
#' @param caption The table caption
#' @param label The table label
#' @param include.rownames Whether or not the rownames should be included
#' 
#' @seealso xtable
#' @examples
#' mktable(baseline, caption="Table 1.", label="")
#' 
#' @export
mktable<-function(x, caption="", label="", include.rownames=T){
 
  # set up shading for alternating rows 
  rws<-seq(1, nrow(x)-1, 2)
  col<-rep("\\rowcolor[gray]{0.9}", length(rws))  
  
  # sensible rounding 
  ndigits<-function(a) (a<50)+(a<10)
  
  # significant digits for rounding, constant for each row 
  rnd<-apply(ndigits(x), 1, max)
  
  # functions to bold and center column headers, bold and left-align row names
  bold.cols <- function(a) paste('\\multicolumn{1}{c}','{\\textbf{',a,'}}', sep ='')
  bold.rows <- function(a) paste('\\multicolumn{1}{l}','{\\textbf{',a,'}}', sep ='')
  
  # create text version of rounded data, retaining trailing zeroes via formatC()
  x2<-x
  
  for(i in 1:nrow(x2)){
    x2[i,]=formatC(as.matrix(x)[i,], format="f", digits=rnd[i])
  }
  
  # create the export table with decimal alignment
  xt<-xtable(x2, format="f", caption=caption, label=label, align=c("l", rep(".", ncol(x2))))
  
  # output the table; use 'booktabs' for proper formatting; table title at top; supress rownames; bold and center column names; suppress latex commentary; keep trailing zeroes; color alternating rows
  print(xt, booktabs=T, table.placement="hb", caption.placement="top", include.rownames=include.rownames,  sanitize.rownames.function=bold.rows, sanitize.colnames.function=bold.cols, comment=F, format.args=list(drop0trailing=F), add.to.row = list(pos = as.list(rws), command = col))
  
  
}
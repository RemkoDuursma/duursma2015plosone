to.pdf <- function(expr, filename, ..., verbose=TRUE) {
  if(!file.exists(dirname(filename)))
    dir.create(dirname(filename), recursive=TRUE)
  if ( verbose )
    cat(sprintf("Creating %s\n", filename))
  pdf(filename, ...)
  on.exit(dev.off())
  eval.parent(substitute(expr))
}

to.eps <- function(expr, filename, ..., verbose=TRUE) {
  if(!file.exists(dirname(filename)))
    dir.create(dirname(filename), recursive=TRUE)
  if ( verbose )
    cat(sprintf("Creating %s\n", filename))
  postscript(filename, ...)
  on.exit(dev.off())
  eval.parent(substitute(expr))
}

# Simple function for placing labels on a figure.
plotlabel <- function(txt, where, inset=0.08, inset.x=inset, inset.y=inset,...){
  u <- par()$usr
  if(grepl("left",where))x <- u[1] + inset.x*(u[2]-u[1])
  if(grepl("right",where))x <- u[2] - inset.x*(u[2]-u[1])
  if(grepl("bottom",where))y <- u[3] + inset.y*(u[4]-u[3])
  if(grepl("top",where))y <- u[4] - inset.y*(u[4]-u[3])
  
  text(x,y,txt,font=2,...)
}


#'@title Add a line to a plot
#'@description As \code{abline}, but with \code{from} and \code{to} arguments. 
#'If a fitted linear regression model is used as asn argument, it uses the min and max values of the data used to fit the model.
#'@param a Intercept (optional)
#'@param b Slope (optional)
#'@param reg A fitted linear regression model (output of \code{\link{lm}}).
#'@param from Draw from this X value
#'@param to Draw to this x value
#'@param \dots Further parameters passed to \code{\link{segments}}
#'@export
ablinepiece <- function(a=NULL,b=NULL,reg=NULL,from=NULL,to=NULL,...){
  
  # Borrowed from abline
  if (!is.null(reg)) a <- reg
  
  if (!is.null(a) && is.list(a)) {
    temp <- as.vector(coefficients(a))
    from <- min(a$model[,2], na.rm=TRUE)
    to <- max(a$model[,2], na.rm=TRUE)
    
    if (length(temp) == 1) {
      a <- 0
      b <- temp
    }
    else {
      a <- temp[1]
      b <- temp[2]
    }
  }
  
  segments(x0=from,x1=to,
           y0=a+from*b,y1=a+to*b,...)
  
}

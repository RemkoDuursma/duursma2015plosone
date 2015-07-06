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

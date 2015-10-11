Library <- function(pkg, ...){
  
  PACK <- .packages(all.available=TRUE)
  pkgc <- deparse(substitute(pkg))
  
  if(pkgc %in% PACK){
    library(pkgc, character.only=TRUE)
  } else {
    install.packages(pkgc, ...)
  }
  
}


get_git_SHA <- function(path){

    if(file.exists(path)){
    pkgrep <- repository(path)
    r <- reflog(pkgrep, verbose=FALSE)
    cur_sha <- substr(r[[1]]@sha, 1, 7)
  } else {
    cur_sha <- "???"
  }

return(cur_sha)
}

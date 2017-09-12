

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

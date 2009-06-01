`as.filename.character` <-
function(x,...){
class(x) <- c("filename",class(x))
attr(x,'extras') <- list(...)
return(x)
}


`format.miDate` <-
function(x,...){
Sys.setenv(TZ = "GMT")
y <- format.Date(x,format="%m/%d/%Y")
y[is.infinite(x)] <- x[is.infinite(x)]
y
}


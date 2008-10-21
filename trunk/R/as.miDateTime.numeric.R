`as.miDateTime.numeric` <-
function(x,...){
as.miDateTime(as.miDate(x),as.miTime(x))
}


constant <- function(x,...)UseMethod('constant')
constant.default <- function(x,within,...)x%nests%within

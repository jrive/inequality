ecdf <- function(obj, x, y){
  sum((obj[,1] < x) & (obj[,2] < y))/nrow(obj)
  }
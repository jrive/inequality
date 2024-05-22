dupe <- function(X,W){
  d <- NCOL(X)
  df <- data.frame(W,X)
  colnames <- c("W",sapply(2:(d+1),function(k){paste("X",(k-1),sep = "")}))
  df <- aggregate(W ~.,data = df,FUN=sum) # aggregate based on duplicates
  W <- df$W/sum(df$W)
  mu <-sapply(1:d,function(k){df[,k]%*%W})
  X <- sapply(1:d,function(k){df[,k]/mu[k]})
  return(list(scale_data=X,weights=W))
}
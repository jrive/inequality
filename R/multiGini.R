multiGini <- function(X,W = rep(1/nrow(X),nrow(X)),n = 1){
  #browser()
  start <- Sys.time()
  d <- ncol(X)
  N <- length(W)
  M <- n*N
  
  U <- matrix(runif(M*d),nrow=M,ncol=d)
  
  Z <- dupe(X,W)
  X <- Z$scale_data
  W <- Z$weights
  
  ot <- vquantile(X=X,W=W,type = "discrete",U=U)
  
  ones <- matrix(rep(1,d*M),nrow=M,ncol =d)
  vQ <- rowSums(X)
  weight <- rowProds(ones - U)
  G <- 1 - ((2^d)/d)*sum(sapply(1:length(ot$mass),function(i){vQ[ot$to[i]]*weight[ot$from[i]]*ot$mass[i]}))
  end <- Sys.time()
  
  return(list(G=G,time=end-start))

}
  
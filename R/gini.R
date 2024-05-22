gini <- function(X,W = rep(1/NROW(X),NROW(X)), delta =rep(2,ncol(X)), n = 1){
  start <- Sys.time()
  
  Z <- dupe(X,W)
  X <- Z$scale_data
  W <- Z$weights
  
  d <- NCOL(X)
  
  if (d==1){
    end <- Sys.time()
    g <- unigini(X,W)
    return(list(G=g,time=end - start))
  }
  
  N <- length(W)
  M <- n*N
  
  U <- matrix(runif(M*d),nrow=M,ncol=d)
  
  ot <- vquantile(X=X,W=W,type = "discrete",U=U)

  oneU <- 1 - U
  prodU <- rowProds(oneU)
  weight <- sapply(1:M,function(m){
    prodU[m]*oneU[m,]^(-1)*(oneU[m,]^(delta-1))
  })
  
  c <- 2^(d-1)/sum(delta^(-1))
  G <- 1 - c*sum(sapply(1:length(ot$mass),function(i){X[ot$to[i],]%*%weight[,ot$from[i]]*ot$mass[i]}))

  end <- Sys.time()
  
  return(list(G=G,time=end-start))

}
  
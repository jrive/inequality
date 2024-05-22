LorenzMap <- function(X,W = rep(1/NROW(X),NROW(X)),R=NULL,n = 1){
  # R := matrix of desired ranks
  start <- Sys.time()
  d <- ncol(X)
  if (is.null(R)){
    level1 <- Re(complex(real = 0.99, imaginary = 0)^(1/d))
    level2 <- Re(complex(real = 0.95, imaginary = 0)^(1/d))
    level3 <- Re(complex(real = 0.90, imaginary = 0)^(1/d))
    R <- as.matrix(rbind(rep(level1,d),rep(level2,d),rep(level3,d)))
  }

  N <- length(W)
  M <- N
  U <- matrix(runif(M*d),nrow=M,ncol=d)
  
  Z <- dupe(X,W)
  X <- Z$scale_data
  W <- Z$weights
  
  ot <- vquantile(X=X,W=W,type = "discrete",U=U)
  
  LR <- sapply(1:nrow(R), function(k){
    rowSums(sapply(1:length(ot$mass),function(i){X[ot$to[i],]*all(U[ot$from[i],]<R[k,])*ot$mass[i]}))
  })

  end <- Sys.time()
  
  return(list(LR=t(LR),time=end-start))
}

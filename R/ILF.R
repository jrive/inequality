ilf <- function(X,W = rep(1/nrow(X),nrow(X)),ot,n){
  # This function calculates the 
  # Inverse Lorenz surface and related objects. This version is based
  # on solving the semi-discrete OT problem using otm2D, an algorithm from
  # the Geogram library.
  #
  # This is similar to the package written for Ghosal & Sen 2019, but it is 
  # able to handle weighted samples. 
  #
  # Inputs: X = bivariate allocation data and mean normalized to one
  #         W = sampling weights of X if not iid-- default is iid
  #         n = # of points to sample from the uniform distribution on [0,1]^2
  #         
  # Output: lmap       = Lorenz map estimates at each R sampled from U[0,1]^2
  #         estimate   = empirical CDF of lmap used for Lorenz curves
  #         Gini       = bivariate Gini calculated as MC integration using R
  #         quantile   = power diagram representing the vector quantile from OT
  #         constraint = areas of the cells of diagram to test if constraint
  #                      is satisfied
  #         flag       = counts how many empty cells resulted
  #         time       = time it took to calculate
  #
  #Summary: Step 1: Calculate L(R_i) for each i = 1,...,n and R_i from U[0,1]^2
  #         Step 2: Calculate the CDF estimator via ecdf using L(R_i). 
  #
  # Step 0: Preliminaries
  #browser()
  start <- Sys.time()
  Z <- dupe(X,W)
  X <- Z$scale_data
  W <- Z$weights
  
  pwd <- vquantile(X,W,type = "semidiscrete")
  constraint <- sapply(which(is.na(pwd$cells)== FALSE), function(j){polyarea(pwd$cells[[j]][,1], pwd$cells[[j]][,2])})
  constraint <- max(sqrt(sum((constraint - W)^2)))
  empty <- length(which(is.na(pwd$cells)))
  N <- length(W)
  # Parallelized to make calculations faster by taking advantage of stronger CPU
  # power in some computers.
  cl <- makeCluster(detectCores()-1, type = "SOCK")
  clusterExport(cl=cl,varlist = c('Lmap','X','W','pwd','st_area','st_intersection','st_sfc','st_polygon','rowProds','N'),envir=environment())
  LR <- do.call(rbind,parLapply(cl,1:100, function(i){Lmap(cbind(runif(1),runif(1)),X,W,pwd$cells,N)}))
  stopCluster(cl)
  
  #browser()
  LR <- LR[complete.cases(LR),]
  # Step 2: Calculate the CDF estimator using L(R_i).
  grid <- as.matrix(CJ(x = seq(0,1,0.005), y = seq(0,1,0.005)))
  Z <- apply(grid,1,function(r){ecdf(LR,r[1],r[2])})
  Z <- matrix(Z, nrow = length(seq(0,1,0.005)))
  # Calculate bivariate Gini coefficient by MC integration using L(R_i) sample
  G <- 1 - 2*sum(colMeans(LR))
  # Done, return a list
  end <- Sys.time()
  list_return <- list(Z,LR,G,pwd,constraint,empty,end - start)
  names(list_return) <- c('estimate','lmap','Gini','quantile','constraint','empty','time')
  return(list_return)
}
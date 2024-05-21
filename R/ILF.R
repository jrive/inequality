ILF <- function(X,W = rep(1/nrow(X),nrow(X)),ot,n){
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
  df <- data.frame(X,W)
  colnames(df) <- c("X1","X2","weight") 
  df <- aggregate(weight ~ X1 + X2, df, sum) # aggregate based on duplicates
  #head(df[duplicated(df[,1:2]),]) # to debug duplicates
  W <- df$weight/sum(df$weight)
  mu <- cbind(df[,1]%*%W, df[,2]%*%W)
  X <- cbind(df[,1]/mu[1],df[,2]/mu[2])
  
  pwd <- ot$pwd
  N <- length(W)
  # Parallelized to make calculations faster by taking advantage of stronger CPU
  # power in some computers.
  cl <- makeCluster(detectCores()-1, type = "SOCK")
  clusterExport(cl=cl,varlist = c('Lmap','X','W','pwd','st_area','st_intersection','st_sfc','st_polygon','rowProds','N'),envir=environment())
  LR <- do.call(rbind,parLapply(cl,1:n, function(i){Lmap(cbind(runif(1),runif(1)),X,W,pwd$cells,N)}))
  stopCluster(cl)
  
  #browser()
  LR <- LR[complete.cases(LR),]
  # Step 2: Calculate the CDF estimator using L(R_i).
  
  eCDF <- function(obj, x, y){
    sum((obj[,1] < x) & (obj[,2] < y))/nrow(obj)}
  grid <- as.matrix(CJ(x = seq(0,1,0.005), y = seq(0,1,0.005)))
  Z <- apply(grid,1,function(r){eCDF(LR,r[1],r[2])})
  Z <- matrix(Z, nrow = length(seq(0,1,0.005)))
  # Calculate bivariate Gini coefficient by MC integration using L(R_i) sample
  G <- 1 - 2*(sum(colMeans(LR)))
  # Done, return a list
  end <- Sys.time()
  list_return <- list(LR,Z,G,pwd,ot$constraint,ot$empty,end - start)
  names(list_return) <- c('lmap','estimate','Gini','quantile','constraint','empty','time')
  return(list_return)
}
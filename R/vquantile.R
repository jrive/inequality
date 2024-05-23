vquantile <- function(X,W,type,U=NULL,n=1){
  #browser()
  if (type == "discrete"){
  M <- nrow(U)
  b <- wpp(X,W)
  a <- wpp(U,rep(1,M)/M)
  ot <- quiet_run(transport(a,b,p=2,method = "shortsimplex"))
  return(ot)
  }

  if (type == "semidiscrete"){
    b <- wpp(scaling.min.max(X),W)
    m <- matrix(rep(1,64^2),nrow = 64,ncol = 64)
    a <- pgrid(m)
    ot <- transport(a,b,p=2)
    return(ot)
  }
  
}
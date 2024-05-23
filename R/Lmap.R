Lmap <- function(r,X,W,cells,N){
  # This function calculates the vector-value of the Lmap at (r1,r2)
  #
  # See ILF.R for details regarding the inputs
  #
  # Forming a polygon object makes intersections easy to calculate
  #browser()
  tryCatch({
  rect <- st_polygon(list(rbind(c(0,0),c(0,r[2]), c(r[1],r[2]), c(r[1],0), c(0,0)))) 
  check <- sapply(1:N, function(k){
    p <- rowProds(sweep(cells[[k]],2,r) < 0)
    return(c(all(p==1),any(p==1)&(!all(p==1))))})

  return(colSums(sweep(X[check[1,],],1,W[check[1,]],"*")) + rowSums(sapply(which(check[2,]==TRUE),function(i){X[i,]*st_area(st_intersection(rect,st_polygon(list(rbind(cells[[i]],cells[[i]][1,])))))})))
  },error = function(e){})
  
  return(c(NA,NA))
}
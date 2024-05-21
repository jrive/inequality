scaling.min.max <- function(data, min = 0, max = 1){
  # This function was found in Francis Hsu's testOTM package written for Ghosal & Sen 2021.
  # It scales the data, preparing it for the semi discrete solver
  
  data = scale(data,
               center = apply(as.matrix(data), 2, min, na.rm = TRUE),
               scale = diff(apply(as.matrix(data), 2, range, na.rm = TRUE)))
  data = data * (max - min) + min
  
  return(data)
}
#'
#'
#' *Berger parker function*
#' 
#' 

berger_parker_hill <- function(sp.matrix) {
  
  berger.parker <- c() # initialize vector
  
  for(i.row in 1:nrow(sp.matrix)) {
    
    entity.vector <- sp.matrix[i.row,] #select row, which is one species
    ratio.entity <- entity.vector/sum(entity.vector)
    berger.row <- 1/max(ratio.entity)
    
    berger.parker <- c(berger.parker, berger.row)
    
  }
  
  return(berger.parker)
  
}

berger_parker <- function(sp.matrix) {
  
  berger.parker <- c() # initialize vector
  
  for(i.row in 1:nrow(sp.matrix)) {
    
    entity.vector <- sp.matrix[i.row,] #select row, which is one species
    ratio.entity <- entity.vector/sum(entity.vector)
    berger.row <- max(ratio.entity)
    
    berger.parker <- c(berger.parker, berger.row)
    
  }
  
  return(berger.parker)
  
}
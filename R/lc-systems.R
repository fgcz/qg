#R 

## This file contains the helpers for deriving the position for various LC systems

#' Implementation of the Water system `M_CLASS48x48`
#'
#' @details
#' works only for 46 vials (one plate); while expecting F7 and F8 are used for
#' clean and qc vials
#' 
#' @param n number of vials
#' 
#' @author Christian Panse <cp@fgcz.ethz.ch> 2024-09-04
#'
#' @return a string vector of vial positions.
#' @export
#'
#' @examples
#' .lcWater(46)
.lcWater <- function(n=46){
  stopifnot(n < 47)
  X <- c("A", "B", "C", "D", "E", "F")
  nY <- 8
  P <- c("1")
  counterPlate <- 1
  counterX <- 0
  counterY <- 0
  
  pos <- rep("", n)
  for (i in 1:n){
    pos[i] <- sprintf("%s:%s,%d", counterPlate, X[counterX + 1], (counterY+1))
    counterY <- counterY + 1
    if (i %% nY == 0){
      counterX <- counterX + 1
      counterY <- 0
    }
    if (i %% (length(X) * nY) == 0){
      counterPlate <- counterPlate + 1 
      counterX <- 0
      counterY <- 0
    }
  }
  pos
}

#' @export
.lcVanquish <- function(n = 10){
  X <- c("A", "B", "C", "D", "E")
  nY <- 9
  P <- c("Y", "R", "B", "G")
  counterPlate <- 1
  counterX <- 0
  counterY <- 0
  
  pos <- rep("", n)
  for (i in 1:n){
    pos[i] <- sprintf("%s:%s%d", counterPlate, X[counterX + 1], (counterY+1))
    
    counterY <- counterY + 1
    
    if (i %% nY == 0){
      counterX <- counterX + 1
      counterY <- 0
    }
    if (i %% (length(X) * nY) == 0){
      counterPlate <- counterPlate + 1 
      counterX <- 0
      counterY <- 0
    }
    
  }
  pos
} 



.lcEksigent <- function(){
  stop("Not implemented yet"))
}


.lcNanoElute <- function(){
}


.lcEvosep <- function(){
  stop("Not implemented yet"))
}

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
#' 
#' @references \url{https://www.waters.com/nextgen/ch/de/products/chromatography/chromatography-systems/acquity-uplc-m-class-system.html}
#'
#' @examples
#' .lcWaters(46)
#' @export
.lcWaters <- function(n = 46){
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

#' Implementation of the Thermo Fisher Scientific Vanquish
#'
#' 
#' @param n number of vials
#' 
#' @author Christian Panse <cp@fgcz.ethz.ch> 2024-07-04
#'
#' @return a string vector of vial positions.
#' 
#' @references \url{https://www.thermofisher.com/ch/en/home/industrial/chromatography/liquid-chromatography-lc/hplc-uhplc-systems/vanquish-core-hplc-system.html}
#'
#' @examples
#' .lcVanquish(96)
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
    pos[i] <- sprintf("%s:%s%d", P[counterPlate], X[counterX + 1], (counterY+1))
    
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

#' validateVanquishPlateNumber
#' @examples
#' qg:::.parseVanquishPlateNumber("1:A4")
#' qg:::.lcVanquish() |> sapply(FUN = qg:::.parseVanquishPlateNumber)
.validateVanquishPlateNumber <- function(x){ 
  L <- c("Y", "R", "B", "G")
  pn <- substr(x, 1, 1) 
  if (pn %in% (1:4 |> as.character())) return(L[as.integer(pn)])
  else if (pn %in% L) return(pn)
  else stop("Invalid plate number", x)
}

.parseVanquishPlateNumber <- function(x){
  .validateVanquishPlateNumber(x)
  sprintf("%s%s", .validateVanquishPlateNumber(x), substr(x, 2, 5))
}


.lcEksigent <- function(){
  stop("Not implemented yet")
}


.lcNanoElute <- function(){
}


.lcEvosep <- function(){
  stop("Not implemented yet")
}

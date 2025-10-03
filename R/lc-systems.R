#R 

## This file contains the helpers for deriving the position for various LC systems

#' Implementation of the Water system `M_CLASS48x48`
#'
#' @details
#' works only for 45 vials (one plate); while expecting positions F6, F7 and F8
#' are used for clean, qc(autoQC03), qc(autoQC01) vials
#' 
#' @param n number of vials
#' 
#' @author Christian Panse <cp@fgcz.ethz.ch> 2024-09-04, Christian Panse <cp@fgcz.ethz.ch>, Claudia 2025-10-02
#'
#' @return a string vector of vial positions.
#' 
#' @references \url{https://www.waters.com/nextgen/ch/de/products/chromatography/chromatography-systems/acquity-uplc-m-class-system.html}
#'
#' @examples
#' .lcWaters(45)
#' # two plates
#' .lcWaters(46)
#' testthat::expect_true(.lcWaters(47)[1] == "1:A,1")
#' testthat::expect_true(.lcWaters(47)[47] == "2:A,2")
#' testthat::expect_true(.lcWaters(47)[45] == "1:F,5")
#' testthat::expect_true(.lcWaters(90)[90] == "2:F,5")
#' testthat::expect_error(.lcWaters(91))
#' @export
.lcWaters <- function(n = 45){
  maxNPlate <- 45 
  stopifnot(n <= 2 * maxNPlate)
  
  # define plate grid
  X <- c("A", "B", "C", "D", "E", "F")
  nY <- 8
  
  counterPlate <- 1
  counterX <- 0
  counterY <- 0
  
  pos <- rep("", n)
  for (i in 1:n){
    pos[i] <- sprintf("%s:%s,%d", counterPlate, X[counterX + 1], (counterY + 1))
    counterY <- counterY + 1
    if (counterY %% nY == 0){
      counterX <- counterX + 1
      counterY <- 0
    }
    #if (i %% (length(X) * nY) == 0){
    if(i >= counterPlate * maxNPlate) {
      counterPlate <- counterPlate + 1 
      counterX <- 0
      counterY <- 0
    }
  } # for
  return(pos)
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
#' .lcVanquish(96, patternLastPos = ":E9")
#' .lcVanquish(96, patternLastPos = ":F6")
#' @export
.lcVanquish <- function(n = 10, patternLastPos = ":E9", availablePlates =  c("Y", "R", "B", "G")){
  X <- c("A", "B", "C", "D", "E", "F")
  nY <- 9

  counterPlate <- 1
  counterX <- 0
  counterY <- 0
  
  pos <- rep("", n)
  for (i in 1:n){
    pos[i] <- sprintf("%s:%s%d", availablePlates[counterPlate], X[counterX + 1], (counterY+1))
    
    counterY <- counterY + 1
    
    if (grepl(pattern = patternLastPos, pos[i])){
      counterPlate <- counterPlate + 1 
      counterX <- 0
      counterY <- 0
    }else if (i %% nY == 0){
      counterX <- counterX + 1
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

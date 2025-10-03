#' @inherit .pooledQC
#' @author Martina 2024-11-05 11:24
#' @noRd
.pooledQCPlate <- function(x, plateId = "Y", QCrow = "H", mode = ""){
  data.frame(matrix(NA, ncol = ncol(x), nrow = 4)) -> pool
  colnames(pool) <- colnames(x)

  pool[1, "File Name"] <- sprintf("{date}_{run}_C{container}_pooledQC%s", mode)
  pool$Position[1] <- sprintf("%s:%s%d", plateId, QCrow, 9)
  pool$`Sample Name`[1] <- sprintf("pooledQC%s", mode)

  pool[2, "File Name"] <- sprintf("{date}_{run}_C{container}_NIST%s", mode)
  pool$Position[2] <- sprintf("%s:%s%d", plateId, QCrow, 11)
  pool$`Sample Name`[2] <- sprintf("NIST-plasma%s", mode)

  pool[3, "File Name"] <- sprintf("{date}_{run}_C{container}_150mix%s", mode)
  pool$Position[3] <- sprintf("%s:%s%d", plateId, QCrow, 12)
  pool$`Sample Name`[3] <- sprintf("150mix%s", mode)

  pool[4, "File Name"] <- sprintf("{date}_{run}_C{container}_blank%s", mode)
  pool$Position[4] <- sprintf("%s:%s%d", plateId, QCrow, sample(2,1))
  pool$`Sample Name`[4] <- sprintf("blank%s", mode)

  pool$`Inj Vol` <- 3.5
  pool
}


.cleanPlate <- function(x, plateId = "Y", QCrow = "H", mode = ""){
  data.frame(matrix(NA, ncol = ncol(x), nrow = 1)) -> pool
  colnames(pool) <- colnames(x)

  pool[1, "File Name"] <- sprintf("{date}_{run}_C{container}_blank%s", mode)
  pool$Position[1] <- sprintf("%s:%s%d", plateId,QCrow, sample(2, 1))
  pool$`Sample Name`[1] <- sprintf("blank%s", mode)

  pool$`Inj Vol` <- 3.5
  pool
}


#' @author Martina 2024-11-05
.pooledQCDilPlate <- function(x, plateId = "Y", QCrow = "H", mode = ""){
  data.frame(matrix(NA, ncol = ncol(x), nrow = 9)) -> pool
  colnames(pool) <- colnames(x)

  for (i in 1:7){
    if (i != 7){
      pool[i, "File Name"] <- sprintf("{date}_{run}_C{container}_pooledQCDil%d%s", i, mode)
    }else{
      pool[i, "File Name"] <- sprintf("{date}_{run}_C{container}_pooledQC%s", mode)
    }

    pool$Position[i] <- sprintf("%s:%s%d", plateId, QCrow,i + 2)
    pool$`Sample Name`[i] <- sprintf("QC dil%d%s", i, mode)
    pool$`Instrument Method`[i] <- "xxxxxx  xxxx  x"
  }

  pool[8, "File Name"] <- sprintf("{date}_{run}_C{container}_150mix%s", mode)
  pool$Position[8] <- sprintf("%s:%s%d", plateId, QCrow, 12)
  pool$`Sample Name`[8] <- sprintf("150mix%s", mode)

  pool[9, "File Name"] <- sprintf("{date}_{run}_C{container}_blank%s", mode)
  pool$Position[9] <- sprintf("%s:%s%d", plateId, QCrow, sample(2,1))
  pool$`Sample Name`[9] <- sprintf("blank%s", mode)

  pool$`Inj Vol` <- 3.5
  pool
}



## TODOs(cp):
## 1. take clean dil qcs only from plateId H?
## 2. insert tube ID.
## 3. dir for instrument method
#' qconfig metabolomics for plates
#'
#' @param x a data.frame
#' @param howOften how frequently the sample should be inserted
#' @param ... parameters to pass to \code{\link[qg]{.insertSample}}
#' 
#' @details
#' as defined my MZ
#' 
#' @return data.frame
#' @export
#' @author Martina CP 2024-11-05
qconfigMetabolomicsVanquishPlateXCaliburSII <- function(x, howOften = 22, ...){
  cn <- c("File Name", "Path", "Position", "Inj Vol", "L3 Laboratory",
          "Sample ID", "Sample Name", "Instrument Method")
  
  # base::save(x, file="/tmp/mx.RData")
  # browser()
  # ignore H row
  x[grepl(pattern = ":[ABCDEFG][1-9]", x = x$Position), ] -> x
  
  message(x$Path[1])

  # in between
  x |> .insertSample(howOften = howOften + 1, sampleFUN = .pooledQCPlate,
                     path = x$Path[1], ...) -> x
  
  # START
  x |> .insertSample(where = 0, sampleFUN = .pooledQCDilPlate, path = x$Path[1], ...) -> x
  x |> .insertSample(where = 0, sampleFUN = .cleanPlate, path = x$Path[1], ...) -> x
  x |> .insertSample(where = 0, sampleFUN = .cleanPlate, path = x$Path[1], ...) -> x
  
  # END
  x |> .insertSample(where = (nrow(x) + 1), sampleFUN = .cleanPlate, path = x$Path[1], ...) -> x
  x |> .insertSample(where = (nrow(x) + 1), sampleFUN = .cleanPlate, path = x$Path[1], ...) -> x
  x |> .insertSample(where = (nrow(x) + 1), sampleFUN = .pooledQCDilPlate, path = x$Path[1], ...) -> x

  x <- qg:::.applyCommonFields(x)
  # x$Position |> sapply(FUN = .parsePlateNumber) -> x$Position
  #x$Position |> sapply(FUN = .parsePlateNumber) -> x$Position
  x[, cn]
}


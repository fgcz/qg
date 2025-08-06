

# Metabolomics ========================================
#' pooledQC
#' @author 2024-07-11 Martina
#' @param x data frame with the samples
#' @noRd
.pooledQC <- function(x, plateId = "Y", QCrow = "H", mode = "", containerid=""){
  #plateId <- x$Position[nrow(x)] |> substr(1,1)
  data.frame(matrix(NA, ncol = ncol(x), nrow = 3)) -> pool
  colnames(pool) <- colnames(x)
  currentdate <- format(Sys.time(), "%Y%m%d")
  
  pool[1, "File Name"] <- sprintf("%s_@@@_C%s_pooledQC%s", currentdate, containerid, mode)
  pool$Position[1] <- sprintf("%s:%s%d", plateId, QCrow, 8)
  pool$`Sample Name`[1] <- sprintf("pooledQC%s", mode)
  
  pool[2, "File Name"] <- sprintf("%s_@@@_C%s_150mix%s", currentdate, containerid, mode)
  pool$Position[2] <- sprintf("%s:%s%d", plateId, QCrow, 9)
  pool$`Sample Name`[2] <- sprintf("150mix%s", mode)
  
  pool[3, "File Name"] <- sprintf("%s_@@@_C%s_blank%s", currentdate, containerid, mode)
  pool$Position[3] <- sprintf("%s:%s%d", plateId, QCrow, 1)
  pool$`Sample Name`[3] <- sprintf("blank%s", mode)
  
  pool$`Inj Vol` <- 3.5
  pool
}

# Metabolomics ========================================
#' @author 2024-07-11 Martina
#' @param x data frame with the samples
#' @noRd
.pooledQCSplash <- function(x, plateId = "Y", QCrow = "H", mode = "", containerid=""){
  #plateId <- x$Position[nrow(x)] |> substr(1,1)
  data.frame(matrix(NA, ncol = ncol(x), nrow = 3)) -> pool
  colnames(pool) <- colnames(x)
  currentdate <- format(Sys.time(), "%Y%m%d")
  
  pool[1, "File Name"] <- sprintf("%s_@@@_C%s_pooledQC%s", currentdate, containerid, mode)
  pool$Position[1] <- sprintf("%s:%s%d", plateId, QCrow, 8)
  pool$`Sample Name`[1] <- sprintf("pooledQC%s", mode)
  
  pool[2, "File Name"] <- sprintf("%s_@@@_C%s_splash%s", currentdate, containerid, mode)
  pool$Position[2] <- sprintf("%s:%s%d", plateId, QCrow, 9)
  pool$`Sample Name`[2] <- sprintf("splash%s", mode)
  
  pool[3, "File Name"] <- sprintf("%s_@@@_C%s_blank%s", currentdate, containerid, mode)
  pool$Position[3] <- sprintf("%s:%s%d", plateId, QCrow, 1)
  pool$`Sample Name`[3] <- sprintf("blank%s", mode)
  
  pool$`Inj Vol` <- 3.5
  pool
}


#' @inherit .pooledQC
#' @author Martina 2024-11-05 11:24
#' @noRd
.pooledQCPlate <- function(x, plateId = "Y", QCrow = "H", mode = "", containerid=""){
  data.frame(matrix(NA, ncol = ncol(x), nrow = 4)) -> pool
  colnames(pool) <- colnames(x)
  currentdate <- format(Sys.time(), "%Y%m%d")
  
  pool[1, "File Name"] <- sprintf("%s_@@@_C%s_pooledQC%s", currentdate, containerid, mode)
  pool$Position[1] <- sprintf("%s:%s%d", plateId, QCrow, 9)
  pool$`Sample Name`[1] <- sprintf("pooledQC%s", mode)

  pool[2, "File Name"] <- sprintf("%s_@@@_C%s_NIST%s", currentdate, containerid, mode)
  pool$Position[2] <- sprintf("%s:%s%d", plateId, QCrow, 11)
  pool$`Sample Name`[2] <- sprintf("NIST-plasma%s", mode)
  
  pool[3, "File Name"] <- sprintf("%s_@@@_C%s_150mix%s", currentdate, containerid, mode)
  pool$Position[3] <- sprintf("%s:%s%d", plateId, QCrow, 12)
  pool$`Sample Name`[3] <- sprintf("150mix%s", mode)

  pool[4, "File Name"] <- sprintf("%s_@@@_C%s_blank%s", currentdate, containerid, mode)
  pool$Position[4] <- sprintf("%s:%s%d", plateId, QCrow, sample(2,1))
  pool$`Sample Name`[4] <- sprintf("blank%s", mode)
  
  pool$`Inj Vol` <- 3.5
  pool
}


.clean <- function(x, plateId = "Y", QCrow = "H", mode = "", containerid=""){
  data.frame(matrix(NA, ncol = ncol(x), nrow = 1)) -> pool
  colnames(pool) <- colnames(x)
  currentdate <- format(Sys.time(), "%Y%m%d")
  
  pool[1, "File Name"] <- sprintf("%s_@@@_C%s_blank%s", currentdate, containerid, mode)
  pool$Position[1] <- sprintf("%s:%s%d", plateId,QCrow, 1)
  pool$`Sample Name`[1] <- sprintf("blank%s", mode)
  
  pool$`Inj Vol` <- 3.5
  pool
}

.cleanPlate <- function(x, plateId = "Y", QCrow = "H", mode = "", containerid = ""){
  data.frame(matrix(NA, ncol = ncol(x), nrow = 1)) -> pool
  colnames(pool) <- colnames(x)
  currentdate <- format(Sys.time(), "%Y%m%d")
  
  pool[1, "File Name"] <- sprintf("%s_@@@_C%s_blank%s", currentdate, containerid, mode)
  pool$Position[1] <- sprintf("%s:%s%d", plateId,QCrow, sample(2, 1))
  pool$`Sample Name`[1] <- sprintf("blank%s", mode)
  
  pool$`Inj Vol` <- 3.5
  pool
}

.pooledQCDil <- function(x, plateId = "Y", QCrow = "H", mode = "", containerid=""){
  data.frame(matrix(NA, ncol = ncol(x), nrow = 9)) -> pool
  colnames(pool) <- colnames(x)
  currentdate <- format(Sys.time(), "%Y%m%d")
  
  for (i in 1:7){
    if (i != 7){
      pool[i, "File Name"] <- sprintf("%s_@@@_C%s_pooledQCDil%d%s", currentdate, containerid, i, mode)
    }else{
      pool[i, "File Name"] <- sprintf("%s_@@@_C%s_pooledQC%s", currentdate, containerid, mode)
    }
    pool$Position[i] <- sprintf("%s:%s%d", plateId, QCrow,i + 1)
    pool$`Sample Name`[i] <- sprintf("QC dil%d%s", i, mode)
    pool$`Instrument Method`[i] <- "xxxxxx  xxxx  x"
  }
  
  pool[8, "File Name"] <- sprintf("%s_@@@_C%s_150mix%s", currentdate, containerid, mode)
  pool$Position[8] <- sprintf("%s:%s%d", plateId, QCrow, 9)
  pool$`Sample Name`[8] <- sprintf("150mix%s", mode)
  
  pool[9, "File Name"] <- sprintf("%s_@@@_C%s_blank%s", currentdate, containerid, mode)
  pool$Position[9] <- sprintf("%s:%s%d", plateId, QCrow, 1)
  pool$`Sample Name`[9] <- sprintf("blank%s", mode)
  
  pool$`Inj Vol` <- 3.5
  pool
}
.pooledQCDilSplash <- function(x, plateId = "Y", QCrow = "H", mode = "", containerid=""){
  data.frame(matrix(NA, ncol = ncol(x), nrow = 9)) -> pool
  colnames(pool) <- colnames(x)
  currentdate <- format(Sys.time(), "%Y%m%d")
  
  for (i in 1:7){
    if (i != 7){
      pool[i, "File Name"] <- sprintf("%s_@@@_C%s_pooledQCDil%d%s", currentdate, containerid, i, mode)
    }else{
      pool[i, "File Name"] <- sprintf("%s_@@@_C%s_pooledQC%s", currentdate, containerid, mode)
    }
    pool$Position[i] <- sprintf("%s:%s%d", plateId, QCrow,i + 1)
    pool$`Sample Name`[i] <- sprintf("QC dil%d%s", i, mode)
    pool$`Instrument Method`[i] <- "xxxxxx  xxxx  x"
  }
  
  pool[8, "File Name"] <- sprintf("%s_@@@_C%s_Splash%s", currentdate, containerid, mode)
  pool$Position[8] <- sprintf("%s:%s%d", plateId, QCrow, 9)
  pool$`Sample Name`[8] <- sprintf("Splash%s", mode)
  
  pool[9, "File Name"] <- sprintf("%s_@@@_C%s_blank%s", currentdate, containerid, mode)
  pool$Position[9] <- sprintf("%s:%s%d", plateId, QCrow, 1)
  pool$`Sample Name`[9] <- sprintf("blank%s", mode)
  
  pool$`Inj Vol` <- 3.5
  pool
}

#' @author Martina 2024-11-05
.pooledQCDilPlate <- function(x, plateId = "Y", QCrow = "H", mode = "", containerid=""){
  data.frame(matrix(NA, ncol = ncol(x), nrow = 9)) -> pool
  colnames(pool) <- colnames(x)
  currentdate <- format(Sys.time(), "%Y%m%d")
  
  for (i in 1:7){
    if (i != 7){
      pool[i, "File Name"] <- sprintf("%s_@@@_C%s_pooledQCDil%d%s", currentdate, containerid, i, mode)
    }else{
      pool[i, "File Name"] <- sprintf("%s_@@@_C%s_pooledQC%s", currentdate, containerid, mode)
    }
    
    pool$Position[i] <- sprintf("%s:%s%d", plateId, QCrow,i + 2)
    pool$`Sample Name`[i] <- sprintf("QC dil%d%s", i, mode)
    pool$`Instrument Method`[i] <- "xxxxxx  xxxx  x"
  }
  
  pool[8, "File Name"] <- sprintf("%s_@@@_C%s_150mix%s", currentdate, containerid, mode)
  pool$Position[8] <- sprintf("%s:%s%d", plateId, QCrow, 12)
  pool$`Sample Name`[8] <- sprintf("150mix%s", mode)
  
  pool[9, "File Name"] <- sprintf("%s_@@@_C%s_blank%s", currentdate, containerid, mode)
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
  im <- paste0(x$Path[1], "\\methods\\")
  
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
  
  x$`L3 Laboratory` <- "FGCZ"
  # x$Position |> sapply(FUN = .parsePlateNumber) -> x$Position
  x$`Instrument Method` <- im
  #x$Position |> sapply(FUN = .parsePlateNumber) -> x$Position
  x[, cn]
}

#' qconfig metabolomics for plates
#'
#' @inheritParams qconfigMetabolomicsVanquishPlateXCaliburSII
#' @export
qconfigMetabolomicsVanquishVialXCaliburSII <- function(x, howOften = 22, ...){
  cn <- c("File Name", "Path", "Position", "Inj Vol", "L3 Laboratory",
          "Sample ID", "Sample Name", "Instrument Method")
  
  # base::save(x, file="/tmp/mx.RData")
  # browser()
  # ignore F (last) row TODO(cp): how to generalize it?
  x[grepl(pattern = ":[ABCDEFG][1-9]", x = x$Position), ] -> x
  
  im <- paste0(x$Path[1], "\\methods\\")
  
  x |> .insertSample(howOften = howOften + 1, sampleFUN = .pooledQC,
                     path = x$Path[1], ...) -> x
  
  # START
  x |> .insertSample(where = 0, sampleFUN = .pooledQCDil, path = x$Path[1], ...) -> x
  x |> .insertSample(where = 0, sampleFUN = .clean, path = x$Path[1], ...) -> x
  x |> .insertSample(where = 0, sampleFUN = .clean, path = x$Path[1], ...) -> x
  
  # END
  x |> .insertSample(where = (nrow(x) + 1), sampleFUN = .clean, path = x$Path[1], ...) -> x
  x |> .insertSample(where = (nrow(x) + 1), sampleFUN = .clean, path = x$Path[1], ...) -> x
  
  x |> .insertSample(where = (nrow(x) + 1), sampleFUN = .pooledQCDil, path = x$Path[1], ...) -> x
  
  x$`L3 Laboratory` <- "FGCZ"
  # x$Position |> sapply(FUN = .parsePlateNumber) -> x$Position
  x$`Instrument Method` <- im
  # x$Position |> sapply(FUN = .parsePlateNumber) -> x$Position
  x[, cn]
}

#' qconfig metabolomics for plates
#'
#' @inheritParams qconfigMetabolomicsVanquishPlateXCaliburSII
#' @export
qconfigMetabolomicsVanquishVialXCaliburSIISPLASH <- function(x, howOften = 22, ...){
  cn <- c("File Name", "Path", "Position", "Inj Vol", "L3 Laboratory",
          "Sample ID", "Sample Name", "Instrument Method")
  
  # base::save(x, file="/tmp/mx.RData")
  # browser()
  # ignore F (last) row TODO(cp): how to generalize it?
  x[grepl(pattern = ":[ABCDEFG][1-9]", x = x$Position), ] -> x
  
  im <- paste0(x$Path[1], "\\methods\\")
  
  x |> .insertSample(howOften = howOften + 1, sampleFUN = .pooledQCSplash,
                     path = x$Path[1], ...) -> x
  
  # START
  x |> .insertSample(where = 0, sampleFUN = .pooledQCDilSplash, path = x$Path[1], ...) -> x
  x |> .insertSample(where = 0, sampleFUN = .clean, path = x$Path[1], ...) -> x
  x |> .insertSample(where = 0, sampleFUN = .clean, path = x$Path[1], ...) -> x
  
  # END
  x |> .insertSample(where = (nrow(x) + 1), sampleFUN = .clean, path = x$Path[1], ...) -> x
  x |> .insertSample(where = (nrow(x) + 1), sampleFUN = .clean, path = x$Path[1], ...) -> x
  
  x |> .insertSample(where = (nrow(x) + 1), sampleFUN = .pooledQCDil, path = x$Path[1], ...) -> x
  
  x$`L3 Laboratory` <- "FGCZ"
  # x$Position |> sapply(FUN = .parsePlateNumber) -> x$Position
  x$`Instrument Method` <- im
  # x$Position |> sapply(FUN = .parsePlateNumber) -> x$Position
  x[, cn]
}


#' qconfig metabolomics for plates
#'
#' @inheritParams qconfigMetabolomicsVanquishPlateXCaliburSII
#' @export
qconfigMetabolomicsVanquishVialXCaliburSII <- function(x, howOften = 22, ...){
  cn <- c("File Name", "Path", "Position", "Inj Vol", "L3 Laboratory",
          "Sample ID", "Sample Name", "Instrument Method")
  
  # base::save(x, file="/tmp/mx.RData")
  # browser()
  # ignore F (last) row TODO(cp): how to generalize it?
  x[grepl(pattern = ":[ABCDEFG][1-9]", x = x$Position), ] -> x
  
  im <- paste0(x$Path[1], "\\methods\\")
  
  x |> .insertSample(howOften = howOften + 1, sampleFUN = .pooledQC,
                     path = x$Path[1], ...) -> x
  
  # START
  x |> .insertSample(where = 0, sampleFUN = .pooledQCDil, path = x$Path[1], ...) -> x
  x |> .insertSample(where = 0, sampleFUN = .clean, path = x$Path[1], ...) -> x
  x |> .insertSample(where = 0, sampleFUN = .clean, path = x$Path[1], ...) -> x
  
  # END
  x |> .insertSample(where = (nrow(x) + 1), sampleFUN = .clean, path = x$Path[1], ...) -> x
  x |> .insertSample(where = (nrow(x) + 1), sampleFUN = .clean, path = x$Path[1], ...) -> x
  
  x |> .insertSample(where = (nrow(x) + 1), sampleFUN = .pooledQCDil, path = x$Path[1], ...) -> x
  
  x$`L3 Laboratory` <- "FGCZ"
  # x$Position |> sapply(FUN = .parsePlateNumber) -> x$Position
  x$`Instrument Method` <- im
  # x$Position |> sapply(FUN = .parsePlateNumber) -> x$Position
  x[, cn]
}

#' qconfig metabolomics for plates
#' 
#' @seealso also test-Metabolomics-Vanquish-VialXCaliburSII-EXPLORIS_4-c37530.R
#' @inheritParams qconfigMetabolomicsVanquishPlateXCaliburSII
#' @export
#' @examples
#' qg:::.readPackageFile('test-Metabolomics-Vanquish-VialXCaliburSII-EXPLORIS_4-c37530.RData') |>
#' qconfigMetabolomicsVanquishVialXCaliburSIIEquiSPLASH()
qconfigMetabolomicsVanquishVialXCaliburSIIEquiSPLASH <- function(x, howOften = 22, ...){
  #write.csv2(x, file ="/tmp/c37530-MetabolomicsVanquishVialXCaliburSIIEquiSPLASH.csv", row.names = FALSE)
  #browser()
  
  cn <- c("File Name", "Path", "Position", "Inj Vol", "L3 Laboratory",
          "Sample ID", "Sample Name", "Instrument Method")
  
  # base::save(x, file="/tmp/mx.RData")
  # browser()
  # ignore F (last) row TODO(cp): how to generalize it?
  x[grepl(pattern = ":[ABCDEFG][1-9]", x = x$Position), ] -> x
  
  im <- paste0(x$Path[1], "\\methods\\")
  
  x |> .insertSample(howOften = howOften + 1, sampleFUN = qg:::.pooledQCSplash,
                     path = x$Path[1], ...) -> x
  
  # START
  x |> .insertSample(where = 0, sampleFUN = .pooledQCDilSplash, path = x$Path[1], ...) -> x
  x |> .insertSample(where = 0, sampleFUN = .clean, path = x$Path[1], ...) -> x
  x |> .insertSample(where = 0, sampleFUN = .clean, path = x$Path[1], ...) -> x
  
  # END
  x |> .insertSample(where = (nrow(x) + 1), sampleFUN = .clean, path = x$Path[1], ...) -> x
  x |> .insertSample(where = (nrow(x) + 1), sampleFUN = .clean, path = x$Path[1], ...) -> x
  
  x |> .insertSample(where = (nrow(x) + 1), sampleFUN = .pooledQCDil, path = x$Path[1], ...) -> x
  
  x$`L3 Laboratory` <- "FGCZ"
  # x$Position |> sapply(FUN = .parsePlateNumber) -> x$Position
  x$`Instrument Method` <- im
  # x$Position |> sapply(FUN = .parsePlateNumber) -> x$Position
  x[, cn]
}


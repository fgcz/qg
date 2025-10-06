#R

#R


#' autoQC01 Mclass Vial XCalibur SII
#'
#' @inheritParams .autoQC01VialXCaliburSII
#'
#' @author Christian Panse <cp@fgcz.ethz.ch> 2025-08-27
#' @return \code{data.frame} object
.autoQC01MclassVialXCaliburSII <- function(x, plateId = "1", QCrow = "F", QCcol = 8,
                                     mode = "", containerid = "", lc = "Vanquish"){
  
 # plateId <- '1'
  message(paste0("autoQC01 Mlass Vial XCalibur SII lc = ", lc, 'hard coded plaeId = ', plateId))
  
  data.frame(matrix(NA, ncol = ncol(x), nrow = 1)) -> pool
  colnames(pool) <- colnames(x)
  currentdate <- format(Sys.time(), "%Y%m%d")
  
  pool[1, "File Name"] <- sprintf("%s_@@@_C%s_autoQC01%s", currentdate, containerid, mode)
  pool[1, "Position"] <- sprintf("%s:%s,%d", plateId, QCrow, QCcol)
  pool[1, "Sample Name"] <- sprintf("autoQC01%s", mode)
  
  pool$`Inj Vol` <- 2
  pool
  
}

#' @inheritParams .autoQC01VialXCaliburSII
.autoQC03MclassVialXCaliburSII <- function(x, plateId = "1", QCrow = "F", QCcol = 7,
                                     mode = "dia", containerid = "", lc = "Vanquish"){
  data.frame(matrix(NA, ncol = ncol(x), nrow = 1)) -> pool
  colnames(pool) <- colnames(x)
  currentdate <- format(Sys.time(), "%Y%m%d")
  
  pool[1, "File Name"] <- sprintf("%s_@@@_C%s_autoQC03%s", currentdate, containerid, mode)
  pool[1, "Position"] <- sprintf("%s:%s,%d", plateId, QCrow, QCcol)
  pool[1, "Sample Name"] <- sprintf("autoQC03%s", mode)
  
  pool$`Inj Vol` <- 1
  pool
}

#' @inheritParams .autoQC01VialXCaliburSII
.cleanMclassVialXCaliburSII <- function(x, plateId = "1", QCrow = "F", QCcol = 6,
                                  mode = "", containerid = "", lc = "Vanquish"){

  data.frame(matrix(NA, ncol = ncol(x), nrow = 1)) -> pool
  colnames(pool) <- colnames(x)
  currentdate <- format(Sys.time(), "%Y%m%d")
  
  pool[1, "File Name"] <- sprintf("%s_@@@_C%s_clean%s", currentdate, containerid, mode)
  pool[1, "Position"] <- sprintf("%s:%s,%d", plateId, QCrow, QCcol)
  pool[1, "Sample Name"] <- sprintf("clean%s", mode)
  
  pool$`Inj Vol` <- 2
  pool
}



#' Proteomics MCLASS Vial XCalibur SII
#'
#' @inheritParams .autoQC01VialXCaliburSII
#'
#' @details
#' sample input see o39408
#'
#' B:F8 autoQC01
#' B:F7 autoQC03
#' B:F6 clean
#'
#' order 39061
#'
#' @inheritParams qconfigMetabolomicsVanquishVialXCaliburSII_pos
#'
#' @inherit qconfigMetabolomicsVanquishVialXCaliburSII_pos return 
#' 
#' @export
#' 
#' @author Christan Panse <cp@fgcz.ethz.ch> 2025-08-27
qconfigProteomicsMclassVialXCaliburSIIdia <- function(x, howOften = 8, ...){
  shiny::showNotification("method qconfigProteomicsMclassVialXCaliburSII new implemented.",
                          duration = 10, type = "warning")
  
  
  cn <- c("File Name", "Path", "Position", "Inj Vol", "L3 Laboratory",
          "Sample ID", "Sample Name", "Instrument Method")
  
  # base::save(x, file="/tmp/mx.RData")
  # browser()
  # ignore F (last) row TODO(cp): how to generalize it?
  x[grepl(pattern = ":[ABCDEFG][,]{0,1}[1-9]", x = x$Position), ] -> x
  
  im <- paste0(x$Path[1], "\\methods\\")
  
  x |> .insertSample(howOften = round(howOften / 2), path = x$Path[1], sampleFUN = .cleanMclassVialXCaliburSII, modOffset = -1, ...) -> x
  x |> .insertSample(howOften = howOften + 2, path = x$Path[1], sampleFUN = .autoQC01MclassVialXCaliburSII, modOffset = -1, ...) -> x
  
  
  # START
  x |> .insertSample(where = 0, path = x$Path[1], sampleFUN = .autoQC01MclassVialXCaliburSII, ...) -> x
  x |> .insertSample(where = 0, path = x$Path[1], sampleFUN = .autoQC03MclassVialXCaliburSII, ...) -> x
  
  # END
  x |> .insertSample(where = (nrow(x) + 1), path = x$Path[1], sampleFUN = .cleanMclassVialXCaliburSII, ...) -> x
  x |> .insertSample(where = (nrow(x) + 1), path = x$Path[1], sampleFUN = .autoQC01MclassVialXCaliburSII, ...) -> x
  x |> .insertSample(where = (nrow(x) + 1), path = x$Path[1], sampleFUN = .autoQC03MclassVialXCaliburSII, ...) -> x
  #x |> .insertSample(where = (nrow(x) + 1), path = x$Path[1], sampleFUN = .cleanMclassVialXCaliburSII, ...) -> x
  
  
  
  x$`L3 Laboratory` <- "FGCZ"
  # x$Position |> sapply(FUN = .parsePlateNumber) -> x$Position
  x$`Instrument Method` <- im
  # x$Position |> sapply(FUN = .parsePlateNumber) -> x$Position
  
  gsub(pattern = ',', replacement = '', x = x$Position) -> x$Position
  
  return(x[, cn])
}
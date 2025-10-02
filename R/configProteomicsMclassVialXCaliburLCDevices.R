#R
## DevDay Claudia/Christian 2025-10-02

#' autoQC01 Mclass Vial XCalibur SII
#'
#' @inheritParams .autoQC01VialXCaliburSII
#'
#' @author Christian Panse <cp@fgcz.ethz.ch> 2025-08-27
#' @return \code{data.frame} object
.autoQC01MclassVialXCaliburLCDevices <- function(x, plateId = "1", QCrow = "F", QCcol = 8,
                                           mode = "dda", containerid = "", lc = "M_CLASS48_48"){
  
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
.autoQC03MclassVialXCaliburLCDevices <- function(x, plateId = "1", QCrow = "F", QCcol = 7,
                                           mode = "dia", containerid = "", lc = "M_CLASS48_48"){
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
.cleanMclassVialXCaliburLCDevices <- function(x, plateId = "1", QCrow = "F", QCcol = 6,
                                        mode = "", containerid = "", lc = "M_CLASS48_48"){
  
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
#' sample input see o39408
#' 
#' B:F,8 autoQC01
#' B:F,7 autoQC03
#' B:F,6 clean
#' 
#' 
#' order 39330
#' 
#' @inheritParams qconfigMetabolomicsVanquishVialXCaliburSIIEquiSPLASH
#' 
#' @inherit qconfigMetabolomicsVanquishVialXCaliburSIIEquiSPLASH return 
#' 
#' @export
#' 
#' @author Christan Panse <cp@fgcz.ethz.ch> 2025-10-02
qconfigProteomicsMclassVialXCaliburLCDevicesDia <- function(x, howOften = 8, ...){
  shiny::showNotification("qconfigProteomicsMclassVialXCaliburLCDevicesDia method is now live.",
                          duration = 10, type = "warning")
  
  
  cn <- c("File Name", "Path", "Position", "Inj Vol", "L3 Laboratory",
          "Sample ID", "Sample Name", "Instrument Method")
  
  # base::save(x, file="/tmp/mx.RData")
  # browser()
  # ignore F (last) row TODO(cp): how to generalize it?
  x[grepl(pattern = ":[ABCDEFG][,]{0,1}[1-9]", x = x$Position), ] -> x
  
  im <- paste0(x$Path[1], "\\methods\\")
  
  #base::save(x, file='/Users/cp/xxx.RData')
  x |> qg:::.insertSample(howOften = howOften / 2, path = x$Path[1], sampleFUN = .cleanMclassVialXCaliburLCDevices, modOffset = -1, ...) -> x
  x |> qg:::.insertSample(howOften = howOften + 2, path = x$Path[1], sampleFUN = .autoQC01MclassVialXCaliburLCDevices, modOffset = -1, ...) -> x
  
  # START
  #x |> .insertSample(where = 0, path = x$Path[1], sampleFUN = .autoQC01MclassVialXCaliburLCDevices, ...) -> x
  #x |> .insertSample(where = 0, path = x$Path[1], sampleFUN = .autoQC03MclassVialXCaliburLCDevices, ...) -> x
  
  # END
  #x |> .insertSample(where = (nrow(x) + 1), path = x$Path[1], sampleFUN = .cleanMclassVialXCaliburLCDevices, ...) -> x
  #x |> .insertSample(where = (nrow(x) + 1), path = x$Path[1], sampleFUN = .autoQC01MclassVialXCaliburLCDevices, ...) -> x
  #x |> .insertSample(where = (nrow(x) + 1), path = x$Path[1], sampleFUN = .autoQC03MclassVialXCaliburLCDevices, ...) -> x
  ##x |> .insertSample(where = (nrow(x) + 1), path = x$Path[1], sampleFUN = .cleanMclassVialXCaliburSII, ...) -> x
  
  
  
  x$`L3 Laboratory` <- "FGCZ"
  # x$Position |> sapply(FUN = .parsePlateNumber) -> x$Position
  x$`Instrument Method` <- im
  # x$Position |> sapply(FUN = .parsePlateNumber) -> x$Position
  
  gsub(pattern = ',', replacement = '', x = x$Position) -> x$Position
  
  return(x[, cn])
}
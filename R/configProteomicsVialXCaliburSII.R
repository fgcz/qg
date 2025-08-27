#R


#' autoQC01 Vial XCaliburSII 
#'
#' @param x 
#' @param plateId e.g., \code{"Y"}.
#' @param QCrow row plate coordinate, e.g., \code{"F"}.
#' @param QCcol column plate coordinate, e.g., \code{9}.
#' @param mode dia or dda 
#' @param containerid 
#' @param lssystem 
#'
#' @author Christian Panse <cp@fgcz.ethz.ch> 2025-08-14
#' @return \code{data.frame} object
.autoQC01VialXCaliburSII <- function(x, plateId = "Y", QCrow = "F", QCcol = 9,
                             mode = "", containerid = "", lc = "Vanquish"){
  
  plateId <- 'B'
  message(paste0("autoQC01VialXCaliburSII lc = ", lc, 'hard coded plaeId = ', plateId))
  
  data.frame(matrix(NA, ncol = ncol(x), nrow = 1)) -> pool
  colnames(pool) <- colnames(x)
  currentdate <- format(Sys.time(), "%Y%m%d")
  
  pool[1, "File Name"] <- sprintf("%s_@@@_C%s_autoQC01%s", currentdate, containerid, mode)
  pool[1, "Position"] <- switch(lc,
                                "M_CLASS48_48" = sprintf("%s:%s,%d", plateId, QCrow, QCcol),
                                "Vanquish"     = sprintf("%s:%s%d", plateId, QCrow, QCcol),
                                sprintf("%s:%s%d", plateId, QCrow = "H")
  )
  pool[1, "Sample Name"] <- sprintf("autoQC01%s", mode)
  
  pool$`Inj Vol` <- 2
  pool
  
}

.autoQC03VialXCaliburSII <- function(x, plateId = "Y", QCrow = "F", QCcol = 8,
                                     mode = "dia", containerid = "", lc = "Vanquish"){
  
  plateId <- 'B'
  message(paste0("autoQC01 lc = ", lc, 'hard coded plaeId = ', plateId))
  
  data.frame(matrix(NA, ncol = ncol(x), nrow = 1)) -> pool
  colnames(pool) <- colnames(x)
  currentdate <- format(Sys.time(), "%Y%m%d")
  
  pool[1, "File Name"] <- sprintf("%s_@@@_C%s_autoQC03%s", currentdate, containerid, mode)
  pool[1, "Position"] <- switch(lc,
                                "M_CLASS48_48" = sprintf("%s:%s,%d", plateId, QCrow, QCcol),
                                "Vanquish"     = sprintf("%s:%s%d", plateId, QCrow, QCcol),
                                sprintf("%s:%s%d", plateId, QCrow = "H")
  )
  pool[1, "Sample Name"] <- sprintf("autoQC03%s", mode)
  
  pool$`Inj Vol` <- 1
  pool
  
}

.cleanVialXCaliburSII <- function(x, plateId = "Y", QCrow = "F", QCcol = 7,
                                     mode = "", containerid = "", lc = "Vanquish"){
  
  plateId <- 'B'
  message(paste0("autoQC01 lc = ", lc, 'hard coded plaeId = ', plateId))
  data.frame(matrix(NA, ncol = ncol(x), nrow = 1)) -> pool
  colnames(pool) <- colnames(x)
  currentdate <- format(Sys.time(), "%Y%m%d")
  
  pool[1, "File Name"] <- sprintf("%s_@@@_C%s_clean%s", currentdate, containerid, mode)
  pool[1, "Position"] <- switch(lc,
                                "M_CLASS48_48" = "1:F,8",
                                "Vanquish"     = sprintf("%s:%s%d", plateId, QCrow, QCcol),
                                sprintf("%s:%s%d", plateId, QCrow = "H")
  )
  pool[1, "Sample Name"] <- sprintf("clean%s", mode)
  
  pool$`Inj Vol` <- 2
  pool
  
}



#' Proteomics Vial XCalibur SII
#'
#' @param x 
#' @param ... 
#'
#' @details similar setup as Astral - Vanquish - SII XCalibur 
#' sample input see o39408
#' B:F9 autoQC01
#' B:F8 autoQC03
#' B:F7 clean
#' 
#' @inheritParams qconfigMetabolomicsVanquishVialXCaliburSIIEquiSPLASH
#' @inherit qconfigMetabolomicsVanquishVialXCaliburSIIEquiSPLASH return 
#' @export
#' @author Antje Dittmann <antje.dittmann@fgcz.uzh.ch>,
#' Pfammatter Sibylle <sibylle.pfammatter@fgcz.ethz.ch>, 
#' Christan Panse <cp@fgcz.ethz.ch> 2025-08-13,14
#' 
#' @examples 
#' S |> qconfigProteomicsVialXCaliburSII()
qconfigProteomicsVialXCaliburSII <- function(x, howOften = 8, ...){
  shiny::showNotification("method qconfigProteomicsVialXCaliburSII new implemented.",
                          duration = 10, type = "warning")
  
  
  cn <- c("File Name", "Path", "Position", "Inj Vol", "L3 Laboratory",
          "Sample ID", "Sample Name", "Instrument Method")
  
  # base::save(x, file="/tmp/mx.RData")
  # browser()
  # ignore F (last) row TODO(cp): how to generalize it?
  x[grepl(pattern = ":[ABCDEFG][,]{0,1}[1-9]", x = x$Position), ] -> x
  
  im <- paste0(x$Path[1], "\\methods\\")
  
  x |> .insertSample(howOften = howOften, path = x$Path[1], sampleFUN = .autoQC01VialXCaliburSII, ...) -> x
  
  # START
  x |> .insertSample(where = 0, path = x$Path[1], sampleFUN = .autoQC03VialXCaliburSII, ...) -> x
  x |> .insertSample(where = 0, path = x$Path[1], sampleFUN = .autoQC01VialXCaliburSII, ...) -> x
 
  
  # END
  x |> .insertSample(where = (nrow(x) + 1), path = x$Path[1], sampleFUN = .cleanVialXCaliburSII, ...) -> x
  x |> .insertSample(where = (nrow(x) + 1), path = x$Path[1], sampleFUN = .autoQC01VialXCaliburSII, ...) -> x
  x |> .insertSample(where = (nrow(x) + 1), path = x$Path[1], sampleFUN = .autoQC03VialXCaliburSII, ...) -> x
  x |> .insertSample(where = (nrow(x) + 1), path = x$Path[1], sampleFUN = .cleanVialXCaliburSII, ...) -> x
  
  
  
  x$`L3 Laboratory` <- "FGCZ"
  # x$Position |> sapply(FUN = .parsePlateNumber) -> x$Position
  x$`Instrument Method` <- im
  # x$Position |> sapply(FUN = .parsePlateNumber) -> x$Position
  
  
  
  return(x[, cn])
}
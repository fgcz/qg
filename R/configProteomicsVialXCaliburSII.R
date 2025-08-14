#R


#' autoQC01VialXCaliburSII autoQC01 
#'
#' @param x 
#' @param plateId 
#' @param QCrow 
#' @param mode 
#' @param containerid 
#' @param lssystem 
#'
#' @author Christian Panse <cp@fgcz.ethz.ch> 2025-08-14
#' @return \code{data.frame} object
.autoQC01VialXCaliburSII <- function(x, plateId = "Y", QCrow = "F", QCcol = 8,
                             mode = "", containerid = "", lc = "Vanquish"){
  
  message(paste0("autoQC01 lc = ", lc))
  data.frame(matrix(NA, ncol = ncol(x), nrow = 1)) -> pool
  colnames(pool) <- colnames(x)
  currentdate <- format(Sys.time(), "%Y%m%d")
  
  pool[1, "File Name"] <- sprintf("%s_@@@_C%s_autoQC01%s", currentdate, containerid, mode)
  pool[1, "Position"] <- switch(lc,
                                "M_CLASS48_48" = "1:F,8",
                                "Vanquish"     = sprintf("%s:%s%d", plateId, QCrow, QCcol),
                                sprintf("%s:%s%d", plateId, QCrow = "H")
  )
  pool[1, "Sample Name"] <- sprintf("autoQC01%s", mode)
  
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
#' 
#' @inheritParams qconfigMetabolomicsVanquishVialXCaliburSIIEquiSPLASH
#' @inherit qconfigMetabolomicsVanquishVialXCaliburSIIEquiSPLASH return 
#' @export
#' @author Antje and Christian
#' 
#' @examples 
qconfigProteomicsVialXCaliburSII <- function(x, howOften = 8, ...){
  shiny::showNotification("method qconfigProteomicsPlateXCaliburSII new implemented.",
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
  x |> .insertSample(where = 0, path = x$Path[1], sampleFUN = .autoQC01VialXCaliburSII, ...) -> x
  
  # END
  x |> .insertSample(where = (nrow(x) + 1), path = x$Path[1], sampleFUN = .autoQC01VialXCaliburSII, ...) -> x
  
  
  x$`L3 Laboratory` <- "FGCZ"
  # x$Position |> sapply(FUN = .parsePlateNumber) -> x$Position
  x$`Instrument Method` <- im
  # x$Position |> sapply(FUN = .parsePlateNumber) -> x$Position
  
  return(x[, cn])
}
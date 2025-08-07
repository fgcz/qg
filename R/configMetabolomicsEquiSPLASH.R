#R



.blankEquiSPLASH <- function(x, plateId = "Y", QCrow = "F", mode = "", containerid=""){
  
  #' take Inj Vol from x
  x[['Inj Vol']][1] -> InjVol
 
  data.frame(matrix(NA, ncol = ncol(x), nrow = 2)) -> pool
  colnames(x) -> colnames(pool)
  format(Sys.time(), "%Y%m%d") ->  currentdate
  
  pool[1, "File Name"] <- sprintf("%s_@@@_C%s_blank_pos", currentdate, containerid)
  pool[2, "File Name"] <- sprintf("%s_@@@_C%s_blank_neg", currentdate, containerid)
  
  
  pool$Position[1] <- sprintf("%s:%s%d", plateId, QCrow, 1)
  pool$Position[2] <- sprintf("%s:%s%d", plateId, QCrow, 1)
  
  pool$`Sample Name`[1] <- sprintf("blank_pos")
  pool$`Sample Name`[2] <- sprintf("blank_neg")
  
  pool$`Inj Vol` <- InjVol
  
  pool
}

#' qconfig metabolomics for plates
#' 
#' @details
#' start with
#' - two blanks pos neg
#' - two EquiSPLASH
#' - two pooledQC
#' - two blanks
#' - two pooledQCDil
#' - two blanks
#' - samples pos neg
#' - ...
#' 
#' 
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
  
  #x |> .insertSample(howOften = howOften + 1, sampleFUN = qg:::.pooledQCSplash,
  #                   path = x$Path[1], ...) -> x
  
  # START
  x |> .insertSample(where = 0, sampleFUN = .blankEquiSPLASH, path = x$Path[1], ...) -> x
  #x |> .insertSample(where = 0, sampleFUN = .clean, path = x$Path[1], ...) -> x
  #x |> .insertSample(where = 0, sampleFUN = .clean, path = x$Path[1], ...) -> x
  
  # END
  #x |> .insertSample(where = (nrow(x) + 1), sampleFUN = .clean, path = x$Path[1], ...) -> x
  #x |> .insertSample(where = (nrow(x) + 1), sampleFUN = .clean, path = x$Path[1], ...) -> x
  
  #x |> .insertSample(where = (nrow(x) + 1), sampleFUN = .pooledQCDil, path = x$Path[1], ...) -> x
  
  x$`L3 Laboratory` <- "FGCZ"
  # x$Position |> sapply(FUN = .parsePlateNumber) -> x$Position
  x$`Instrument Method` <- im
  # x$Position |> sapply(FUN = .parsePlateNumber) -> x$Position
  x[, cn]
}


#R






.alternatingPosNegSample <- function(x, ...){
  # Implementation by Claude Code
  
  x_pos <- x
  x_neg <- x
  
  x_pos[["File Name"]] <- paste0(x[["File Name"]], "_pos")
  x_neg[["File Name"]] <- paste0(x[["File Name"]], "_neg")
  
  x_pos[["Sample Name"]] <- paste0(x[["Sample Name"]], "_pos")
  x_neg[["Sample Name"]] <- paste0(x[["Sample Name"]], "_neg")
  
  # TODO(cp): perform the same thing for the methods
  
  result <- data.frame(matrix(NA, nrow = 2 * nrow(x), ncol = ncol(x)))
  colnames(result) <- colnames(x)
  
  for(i in 1:nrow(x)) {
    x_pos[i, ] -> result[2 * i - 1, ]  
    x_neg[i, ] -> result[2 * i, ] 
  }
  
  result
  
}





#' qconfig metabolomics for vials EquiSPLASH pos_neg
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
#' qg:::.readPackageFile('test-Metabolomics-Vanquish-VialXCaliburSII-EXPLORIS_4-c37530.RData') -> S
#' S|> qconfigMetabolomicsVanquishVialXCaliburSIIEquiSPLASH(containerid = 37530) |> View()
qconfigMetabolomicsVanquishVialXCaliburSIIEquiSPLASH <- function(x, howOften = 8, mode = '', ...){
  #write.csv2(x, file ="/tmp/c37530-MetabolomicsVanquishVialXCaliburSIIEquiSPLASH.csv", row.names = FALSE)
  #browser()
  
  cn <- c("File Name", "Path", "Position", "Inj Vol", "L3 Laboratory",
          "Sample ID", "Sample Name", "Instrument Method")
  
  # base::save(x, file="/tmp/mx.RData")
  # browser()
  # ignore F (last) row TODO(cp): how to generalize it?
  x[grepl(pattern = ":[ABCDEFG][1-9]", x = x$Position), ] -> x

  ########################
  x |> qg::.insertSample(howOften = howOften + 1, sampleFUN = .metabolomicsBlockStandardPoolQC, path = x$Path[1], ...) -> x
  x |> qg::.insertSample(howOften = 2 * (howOften +1 ), sampleFUN = .metabolomicsBlockPooledQCDilution, path = x$Path[1], ...) -> x
  #x |> qg::.insertSample(howOften = howOften + 1, sampleFUN =  .pooledQCDilEquiSPLASH, path = x$Path[1], ...) -> x
  
  # START
  x |> qg::.insertSample(where = 0, sampleFUN = .blankEquiSPLASH, path = x$Path[1], ...) -> x
  x |> qg::.insertSample(where = 0, sampleFUN = .metabolomicsBlockPooledQCDilution, path = x$Path[1], ...) -> x
  x |> qg::.insertSample(where = 0, sampleFUN = .blankEquiSPLASH, path = x$Path[1], ...) -> x
  x |> qg::.insertSample(where = 0, sampleFUN = .pooledQCEquiSPLASH, path = x$Path[1], ...) -> x
  x |> qg::.insertSample(where = 0, sampleFUN = .EquiSPLASH, path = x$Path[1], ...) -> x
  x |> qg::.insertSample(where = 0, sampleFUN = .blankEquiSPLASH, path = x$Path[1], ...) -> x
  x |> qg::.insertSample(where = 0, sampleFUN = .blankEquiSPLASH, path = x$Path[1], ...) -> x
  
 
  # END

  x |> qg::.insertSample(where = (nrow(x) + 1), sampleFUN =  .metabolomicsBlockStandardPoolQC, path = x$Path[1], ...) -> x
  x |> qg::.insertSample(where = (nrow(x) + 1), sampleFUN = .blankEquiSPLASH, path = x$Path[1], ...) -> x
 


  ########################
  x <- qg:::.applyCommonFields(x)
  # x$Position |> sapply(FUN = .parsePlateNumber) -> x$Position
  # x$Position |> sapply(FUN = .parsePlateNumber) -> x$Position

  .alternatingPosNegSample(x) -> x
  x[, cn]
}


  

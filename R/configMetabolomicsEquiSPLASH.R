#R




.pooledQCDilEquiSPLASH <- function(x, plateId = "Y", QCrow = "H", mode = '', path = '???'){
  data.frame(matrix(NA, ncol = ncol(x), nrow = 6)) -> pool

  colnames(pool) <- colnames(x)

  x[['Inj Vol']][1] -> InjVol

  for (i in 1:6){

    pool[i, "File Name"] <- sprintf("{date}_{run}_C{container}_pooledQCDil%d", i)

    pool$Position[i] <- sprintf("%s:%s%d", plateId, QCrow,i + 1)
    pool$`Sample Name`[i] <- sprintf("QC dil%d", i)
    pool$`Instrument Method`[i] <- "xxxxxx  xxxx  x"
  }

  pool$`Inj Vol` <- InjVol
  pool
}


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


.EquiSPLASHrep <- function(x, standard, plateId = "Y"){
  data.frame(matrix(NA, ncol = ncol(x), nrow = 3)) -> pool
  colnames(pool) <- colnames(x)

  pool[1, ] <- .blankMetabolomics(x, plateId = plateId)
  pool[2, ] <- .standardMetabolomics(x, plateId = plateId, standard = standard)
  pool[3, ] <- .pooledQCMetabolomics(x, plateId = plateId)

  pool
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
  x |> qg::.insertSample(howOften = howOften + 1, sampleFUN = .EquiSPLASHrep, path = x$Path[1], ...) -> x
  x |> qg::.insertSample(howOften = 2 * (howOften +1 ), sampleFUN = .pooledQCDilEquiSPLASH, path = x$Path[1], ...) -> x
  #x |> qg::.insertSample(howOften = howOften + 1, sampleFUN =  .pooledQCDilEquiSPLASH, path = x$Path[1], ...) -> x
  
  # START
  x |> qg::.insertSample(where = 0, sampleFUN = .blankEquiSPLASH, path = x$Path[1], ...) -> x
  x |> qg::.insertSample(where = 0, sampleFUN = .pooledQCDilEquiSPLASH, path = x$Path[1], ...) -> x
  x |> qg::.insertSample(where = 0, sampleFUN = .blankEquiSPLASH, path = x$Path[1], ...) -> x
  x |> qg::.insertSample(where = 0, sampleFUN = .pooledQCEquiSPLASH, path = x$Path[1], ...) -> x
  x |> qg::.insertSample(where = 0, sampleFUN = .EquiSPLASH, path = x$Path[1], ...) -> x
  x |> qg::.insertSample(where = 0, sampleFUN = .blankEquiSPLASH, path = x$Path[1], ...) -> x
  x |> qg::.insertSample(where = 0, sampleFUN = .blankEquiSPLASH, path = x$Path[1], ...) -> x
  
 
  # END

  x |> qg::.insertSample(where = (nrow(x) + 1), sampleFUN =  .EquiSPLASHrep, path = x$Path[1], ...) -> x
  x |> qg::.insertSample(where = (nrow(x) + 1), sampleFUN = .blankEquiSPLASH, path = x$Path[1], ...) -> x
 


  ########################
  x <- qg:::.applyCommonFields(x)
  # x$Position |> sapply(FUN = .parsePlateNumber) -> x$Position
  # x$Position |> sapply(FUN = .parsePlateNumber) -> x$Position

  .alternatingPosNegSample(x) -> x
  x[, cn]
}


  

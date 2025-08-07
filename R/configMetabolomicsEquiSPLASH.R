#R

.blankEquiSPLASH <- function(x, plateId = "Y", QCrow = "F", containerid = ""){
  
  #' take Inj Vol from x
  x[['Inj Vol']][1] -> InjVol
 
  data.frame(matrix(NA, ncol = ncol(x), nrow = 1)) -> pool
  colnames(x) -> colnames(pool)
  format(Sys.time(), "%Y%m%d") ->  currentdate
  
  pool[1, "File Name"] <- sprintf("%s_@@@_C%s_blank", currentdate, containerid)
   
  
  pool$Position[1] <- sprintf("%s:%s%d", plateId, QCrow, 1)
  
  pool$`Sample Name`[1] <- sprintf("blank")
 
  pool$`Inj Vol` <- InjVol
  
  pool
}

.EquiSPLASH <- function(x, plateId = "Y", QCrow = "F", containerid = ""){
  
  #' take Inj Vol from x
  x[['Inj Vol']][1] -> InjVol
  
  data.frame(matrix(NA, ncol = ncol(x), nrow = 1)) -> pool
  colnames(x) -> colnames(pool)
  format(Sys.time(), "%Y%m%d") ->  currentdate
  
  pool[1, "File Name"] <- sprintf("%s_@@@_C%s_EquiSPLASH", currentdate, containerid)
  
  
  pool$Position[1] <- sprintf("%s:%s%d", plateId, QCrow, 9)
  
  pool$`Sample Name`[1] <- sprintf("EquiSPLASH")
  
  pool$`Inj Vol` <- InjVol
  
  pool
}

.pooledQCDilEquiSPLASH <- function(x, plateId = "Y", QCrow = "H", mode = "", containerid=""){
  data.frame(matrix(NA, ncol = ncol(x), nrow = 6)) -> pool
  
  colnames(pool) <- colnames(x)
  currentdate <- format(Sys.time(), "%Y%m%d")
  
  x[['Inj Vol']][1] -> InjVol
  
  for (i in 1:6){
    
    pool[i, "File Name"] <- sprintf("%s_@@@_C%s_pooledQCDil%d%s", currentdate, containerid, i, mode)
    
    pool$Position[i] <- sprintf("%s:%s%d", plateId, QCrow,i + 1)
    pool$`Sample Name`[i] <- sprintf("QC dil%d%s", i, mode)
    pool$`Instrument Method`[i] <- "xxxxxx  xxxx  x"
  }
  
  pool$`Inj Vol` <- InjVol
  pool
}


.alternatingPosNegSample <- function(x){
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
#' qconfigMetabolomicsVanquishVialXCaliburSIIEquiSPLASH(containerid = 37530)
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
  x |> .insertSample(where = 0, sampleFUN = .pooledQCDilEquiSPLASH, path = x$Path[1], ...) -> x
  x |> .insertSample(where = 0, sampleFUN = .blankEquiSPLASH, path = x$Path[1], ...) -> x
  x |> .insertSample(where = 0, sampleFUN = .EquiSPLASH, path = x$Path[1], ...) -> x
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
  
  .alternatingPosNegSample(x) -> x
  x[, cn]
}


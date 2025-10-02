
#' Generic queue function for metabolomics
#' 
#' @param x input data frame w
.metabolomicsQueueVial <- function(x, polarities) {
  cn <- c("File Name", "Path", "Position", "Inj Vol", "L3 Laboratory", "Sample ID", "Sample Name", "Instrument Method")
  
  # TODO still needed?
  x <- x[grepl(pattern = ":[ABCDEFG][1-9]", x = x$Position), ]
  im <- paste0(x$Path[1], "\\methods\\")

  ########################
  howOften = 8
  x <- qg::.insertSample(x, howOften = howOften + 1, sampleFUN = .EquiSPLASHrep, path = x$Path[1])
  x <- qg::.insertSample(x, howOften = 2 * (howOften + 1), sampleFUN = .pooledQCDilEquiSPLASH, path = x$Path[1])

  # START
  x <- qg::.insertSample(x, where = 0, sampleFUN = .blankEquiSPLASH, path = x$Path[1])
  x <- qg::.insertSample(x, where = 0, sampleFUN = .pooledQCDilEquiSPLASH, path = x$Path[1])
  x <- qg::.insertSample(x, where = 0, sampleFUN = .blankEquiSPLASH, path = x$Path[1])
  x <- qg::.insertSample(x, where = 0, sampleFUN = .pooledQCEquiSPLASH, path = x$Path[1])
  x <- qg::.insertSample(x, where = 0, sampleFUN = .EquiSPLASH, path = x$Path[1])
  x <- qg::.insertSample(x, where = 0, sampleFUN = .blankEquiSPLASH, path = x$Path[1])
  x <- qg::.insertSample(x, where = 0, sampleFUN = .blankEquiSPLASH, path = x$Path[1])


  # END
  x <- qg::.insertSample(x, where = (nrow(x) + 1), sampleFUN = .EquiSPLASHrep, path = x$Path[1])
  x <- qg::.insertSample(x, where = (nrow(x) + 1), sampleFUN = .blankEquiSPLASH, path = x$Path[1])


  ########################
  x$`L3 Laboratory` <- "FGCZ"
  x$`Instrument Method` <- im

  # TODO add polarities
  x <- .metabolomicsInstantiatePolarities(x, polarities)
  x[, cn]
}

.metabolomicsInstantiatePolarities <- function(x, polarities) {
  # Create empty output data frame with same structure as x
  res <- head(x, 0)

  # Add the rows for each polarity of the input rows
  for (row in 1:nrow(x)) {
    for (polarity in polarities) {
      new_row <- x[row, ]
      new_row[["File Name"]] <- paste0(x[row, "File Name"], "_", polarity)
      res <- rbind(res, new_row)
    }
  }

  return(res)
}

#' @export
qconfigMetabolomicsVanquishVialXCaliburSII_pos <- function(x, howOften = 8, ...) {
  .metabolomicsQueueVial(x, polarities = c("pos"))
}

#' @export
qconfigMetabolomicsVanquishVialXCaliburSII_neg <- function(x, howOften = 8, ...) {
  .metabolomicsQueueVial(x, polarities = c("neg"))
}

#' @export
qconfigMetabolomicsVanquishVialXCaliburSII_pos_neg <- function(x, howOften = 8, ...) {
  .metabolomicsQueueVial(x, polarities = c("pos", "neg"))
}
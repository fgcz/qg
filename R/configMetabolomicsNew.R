#' Generic queue function for metabolomics
#' 
#' @param x input data frame w
.metabolomicsQueueVial <- function(x, howOften, polarities, standard) {
  cn <- c("File Name", "Path", "Position", "Inj Vol", "L3 Laboratory", "Sample ID", "Sample Name", "Instrument Method")
  
  # TODO still needed?
  x <- x[grepl(pattern = ":[ABCDEFG][1-9]", x = x$Position), ]
  im <- paste0(x$Path[1], "\\methods\\")

  ########################
  x <- qg::.insertSample(x, howOften = howOften + 1, sampleFUN = .EquiSPLASHrep, path = x$Path[1], standard = standard)
  x <- qg::.insertSample(x, howOften = 2 * (howOften + 1), sampleFUN = .pooledQCDilEquiSPLASH, path = x$Path[1])

  # START
  x <- qg::.insertSample(x, where = 0, sampleFUN = .blankMetabolomics, path = x$Path[1])
  x <- qg::.insertSample(x, where = 0, sampleFUN = .pooledQCDilEquiSPLASH, path = x$Path[1])
  x <- qg::.insertSample(x, where = 0, sampleFUN = .blankMetabolomics, path = x$Path[1])
  x <- qg::.insertSample(x, where = 0, sampleFUN = .pooledQCMetabolomics, path = x$Path[1])
  x <- qg::.insertSample(x, where = 0, sampleFUN = .standardMetabolomics, path = x$Path[1], standard = standard)
  x <- qg::.insertSample(x, where = 0, sampleFUN = .blankMetabolomics, path = x$Path[1])
  x <- qg::.insertSample(x, where = 0, sampleFUN = .blankMetabolomics, path = x$Path[1])

  # END
  x <- qg::.insertSample(x, where = (nrow(x) + 1), sampleFUN = .EquiSPLASHrep, path = x$Path[1], standard = standard)
  x <- qg::.insertSample(x, where = (nrow(x) + 1), sampleFUN = .blankMetabolomics, path = x$Path[1])

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

# NOTE: These are all vial configs

#' @export
qconfigMetabolomicsVanquishVialXCaliburSII_pos <- function(x, howOften, ...) {
  .metabolomicsQueueVial(x, howOften = howOften, polarities = c("pos"), standard = "108mix")
}

#' @export
qconfigMetabolomicsVanquishVialXCaliburSII_neg <- function(x, howOften, ...) {
  .metabolomicsQueueVial(x, howOften = howOften, polarities = c("neg"), standard = "108mix")
}

#' @export
qconfigMetabolomicsVanquishVialXCaliburSII_pos_neg <- function(x, howOften, ...) {
  .metabolomicsQueueVial(x, howOften = howOften, polarities = c("pos", "neg"), standard = "108mix")
}

#' @export
qconfigLipidomicsVanquishVialXCaliburSII_pos <- function(x, howOften, ...) {
  .metabolomicsQueueVial(x, howOften = howOften, polarities = c("pos"), standard = "EquiSPLASH")
}

#' @export
qconfigLipidomicsVanquishVialXCaliburSII_neg <- function(x, howOften, ...) {
  .metabolomicsQueueVial(x, howOften = howOften, polarities = c("neg"), standard = "EquiSPLASH")
}

#' @export
qconfigLipidomicsVanquishVialXCaliburSII_pos_neg <- function(x, howOften, ...) {
  .metabolomicsQueueVial(x, howOften = howOften, polarities = c("pos", "neg"), standard = "EquiSPLASH")
}

# NOTE: These are all plate configs

# TODO to be implemented



#' One blank sample
#'
#' @param x \code{data.frame} contains sample information, e.g., "Sample Name", "Sample ID", "Tube ID", "File Name", "Path", "Position" 
#' @param plateId plate of the sample.
#' @param QCrow plate row of the sample.
#' @param QCcol plate col of the sample.
#' @param containerid bfabric container, order or project id.
#' @param ... 
#' 
#' @author Christian Panse <cp@fgcz.ethz.ch>, 2025-08-13
#' @returns \code{data.frame}
#' @export
#'
#' @examples
.blankMetabolomics <- function(x, plateId = "Y", QCrow = "F"){
  pool <- data.frame(matrix(NA, ncol = ncol(x), nrow = 1))
  colnames(pool) <- colnames(x)

  pool$`File Name` <- "{date}_{run}_C{container}_blank"
  pool$Position <- sprintf("%s:%s%d", plateId, QCrow, 1)
  pool$`Sample Name` <- "blank"
  pool$`Inj Vol` <- x[['Inj Vol']][1]

  pool
}


.standardsConfig <- data.frame(
  standard = c("EquiSPLASH", "108mix_AA", "108mix_OAP"),
  plateId = c("Y", "Y", "Y"),
  row = c("F", "F", "E"),
  col = c(9, 9, 9)
)


.standardMetabolomics <- function(x, plateId = "Y", QCrow = "F", standard = "EquiSPLASH"){
  # Determine which standards to use
  if (standard == "108mix") {
    standards_to_use <- c("108mix_AA", "108mix_OAP")
  } else {
    standards_to_use <- standard
  }

  # Look up config for each standard
  configs <- .standardsConfig[.standardsConfig$standard %in% standards_to_use, , drop = FALSE]

  # Reset row names to avoid indexing issues
  rownames(configs) <- NULL

  # Create output data frame with correct number of rows
  n_standards <- nrow(configs)
  pool <- data.frame(matrix(NA, ncol = ncol(x), nrow = n_standards))
  colnames(pool) <- colnames(x)

  # Fill in values from config for each standard
  for (i in 1:n_standards) {
    pool$`File Name`[i] <- sprintf("{date}_{run}_C{container}_%s", configs$standard[i])
    pool$Position[i] <- sprintf("%s:%s%d", configs$plateId[i], configs$row[i], configs$col[i])
    pool$`Sample Name`[i] <- configs$standard[i]
    pool$`Inj Vol`[i] <- x[['Inj Vol']][1]
  }

  pool
}

.pooledQCMetabolomics <- function(x, plateId = "Y", QCrow = "F", ...){
  pool <- data.frame(matrix(NA, ncol = ncol(x), nrow = 1))
  colnames(pool) <- colnames(x)

  pool$`File Name` <- "{date}_{run}_C{container}_pooledQC"
  pool$Position <- sprintf("%s:%s%d", plateId, QCrow, 8)
  pool$`Sample Name` <- "blank"
  pool$`Inj Vol` <- x[['Inj Vol']][1]

  pool
}
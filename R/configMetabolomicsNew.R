#' Generic queue configuration for metabolomics workflows
#'
#' Since at the queue level, the only difference between metabolomics and lipidomics
#' is the standard, setting the standard parameter is sufficient to cover both.
#'
#' @param x input data frame with primary samples and column structure
#' @param howOften integer, how often to insert QC samples
#' @param polarities character vector, polarities to include
#' @param standard character, standard to use
.metabolomicsQueue <- function(x, howOften, polarities, standard) {
  # START section
  start_section <- rbind(
    .blankMetabolomics(x),
    .metabolomicsBlockStandardPoolQC(x, standard = standard),
    .blankMetabolomics(x),
    .metabolomicsBlockPooledQCDilution(x),
    .blankMetabolomics(x)
  )

  # Main samples with periodic QC insertions
  main_section <- qg::.insertSample(
    x,
    howOften = howOften + 1,
    sampleFUN = .metabolomicsBlockStandardPoolQC,
    standard = standard
  )
  main_section <- qg::.insertSample(
    main_section,
    howOften = 2 * (howOften + 1),
    sampleFUN = .metabolomicsBlockPooledQCDilution,
  )

  # END section
  end_section <- rbind(
    .metabolomicsBlockStandardPoolQC(x, standard = standard),
    .blankMetabolomics(x)
  )

  # Combine all sections
  result <- rbind(start_section, main_section, end_section)
  result$Path <- x$Path[1]

  # Add metadata
  result$`L3 Laboratory` <- "FGCZ"
  result$`Instrument Method` <- sprintf("%s\\methods\\", result$Path)

  # Process polarities
  result <- .metabolomicsInstantiatePolarities(result, polarities)

  # Return specified columns
  out_cols <- c(
    "File Name",
    "Path",
    "Position",
    "Inj Vol",
    "L3 Laboratory",
    "Sample ID",
    "Sample Name",
    "Instrument Method"
  )
  result[, out_cols]
}

.metabolomicsInstantiatePolarities <- function(x, polarities) {
  # Create empty output data frame with same structure as x
  res <- head(x, 0)

  # Add the rows for each polarity of the input rows
  for (row in seq_len(nrow(x))) {
    for (polarity in polarities) {
      new_row <- x[row, ]
      new_row[["File Name"]] <- paste0(x[row, "File Name"], "_", polarity)
      res <- rbind(res, new_row)
    }
  }

  res
}


#' @export
qconfigMetabolomicsVanquishVialXCaliburSII_pos <- function(x, howOften) {
  .metabolomicsQueue(x, howOften = howOften, polarities = c("pos"), standard = "108mix")
}

#' @export
qconfigMetabolomicsVanquishVialXCaliburSII_neg <- function(x, howOften) {
  .metabolomicsQueue(x, howOften = howOften, polarities = c("neg"), standard = "108mix")
}

#' @export
qconfigMetabolomicsVanquishVialXCaliburSII_pos_neg <- function(x, howOften) {
  .metabolomicsQueue(x, howOften = howOften, polarities = c("pos", "neg"), standard = "108mix")
}

#' @export
qconfigLipidomicsVanquishVialXCaliburSII_pos <- function(x, howOften) {
  .metabolomicsQueue(x, howOften = howOften, polarities = c("pos"), standard = "EquiSPLASH")
}

#' @export
qconfigLipidomicsVanquishVialXCaliburSII_neg <- function(x, howOften) {
  .metabolomicsQueue(x, howOften = howOften, polarities = c("neg"), standard = "EquiSPLASH")
}

#' @export
qconfigLipidomicsVanquishVialXCaliburSII_pos_neg <- function(x, howOften) {
  .metabolomicsQueue(x, howOften = howOften, polarities = c("pos", "neg"), standard = "EquiSPLASH")
}

#' @export
qconfigMetabolomicsVanquishPlateXCaliburSII_pos <- function(x, howOften) {
  .metabolomicsQueue(x, howOften = howOften, polarities = c("pos"), standard = "108mix")
}

#' @export
qconfigMetabolomicsVanquishPlateXCaliburSII_neg <- function(x, howOften) {
  .metabolomicsQueue(x, howOften = howOften, polarities = c("neg"), standard = "108mix")
}

#' @export
qconfigMetabolomicsVanquishPlateXCaliburSII_pos_neg <- function(x, howOften) {
  .metabolomicsQueue(x, howOften = howOften, polarities = c("pos", "neg"), standard = "108mix")
}

#' @export
qconfigLipidomicsVanquishPlateXCaliburSII_pos <- function(x, howOften) {
  .metabolomicsQueue(x, howOften = howOften, polarities = c("pos"), standard = "EquiSPLASH")
}

#' @export
qconfigLipidomicsVanquishPlateXCaliburSII_neg <- function(x, howOften) {
  .metabolomicsQueue(x, howOften = howOften, polarities = c("neg"), standard = "EquiSPLASH")
}

#' @export
qconfigLipidomicsVanquishPlateXCaliburSII_pos_neg <- function(x, howOften) {
  .metabolomicsQueue(x, howOften = howOften, polarities = c("pos", "neg"), standard = "EquiSPLASH")
}

#' Generic builder to create metabolomics sample rows from config
#' @param x input data frame with column structure
#' @param sample_types vector of sample type names to create
.createMetabolomicsSample <- function(x, sample_types) {
  # Look up configs for requested sample types
  configs <- .metabolomicsSampleConfig[
    .metabolomicsSampleConfig$sample_type %in% sample_types,
    ,
    drop = FALSE
  ]

  if (nrow(configs) == 0) {
    stop(
      "No configurations found for sample types: ",
      paste(sample_types, collapse = ", ")
    )
  }

  # Create output data frame with correct number of rows
  n_samples <- nrow(configs)
  result <- x[0, ][rep(1, n_samples), ]

  # Fill in values from config for each sample
  for (i in 1:n_samples) {
    result$`File Name`[i] <- configs$file_name_template[i]
    result$Position[i] <- sprintf(
      "%s:%s%d",
      configs$plate_id[i],
      configs$row[i],
      configs$col[i]
    )
    result$`Sample Name`[i] <- configs$sample_name[i]
    result$`Inj Vol`[i] <- x[['Inj Vol']][1]
  }

  result
}

.blankMetabolomics <- function(x) {
  .createMetabolomicsSample(x, "blank")
}

.standardMetabolomics <- function(x, standard) {
  # Determine which standards to use
  if (standard == "108mix") {
    standards_to_use <- c("108mix_AA", "108mix_OAP")
  } else {
    standards_to_use <- standard
  }

  .createMetabolomicsSample(x, standards_to_use)
}

.pooledQCMetabolomics <- function(x) {
  .createMetabolomicsSample(x, "pooledQC")
}

.metabolomicsBlockPooledQCDilution <- function(x, plateId) {
  # TODO plateId is ignored, but necessary for insertSample
  dilution_types <- paste0("pooledQCDil", 1:6)
  .createMetabolomicsSample(x, dilution_types)
}

.metabolomicsBlockStandardPoolQC <- function(x, standard, plateId) {
  # TODO plateId is ignored, but necessary for insertSample
  blank <- .blankMetabolomics(x)
  std <- .standardMetabolomics(x, standard = standard)
  pooledQC <- .pooledQCMetabolomics(x)
  rbind(blank, std, pooledQC)
}

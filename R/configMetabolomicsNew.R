#' Generic queue function for metabolomics
#'
#' @param x input data frame w
.metabolomicsQueueVial <- function(x, howOften, polarities, standard) {
  path <- x$Path[1]

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
    path = path,
    standard = standard
  )
  main_section <- qg::.insertSample(
    main_section,
    howOften = 2 * (howOften + 1),
    sampleFUN = .metabolomicsBlockPooledQCDilution,
    path = path
  )

  # END section
  end_section <- rbind(
    .metabolomicsBlockStandardPoolQC(x, standard = standard),
    .blankMetabolomics(x)
  )

  # Combine all sections
  result <- rbind(start_section, main_section, end_section)
  result$Path <- path

  # Add metadata
  result$`L3 Laboratory` <- "FGCZ"
  result$`Instrument Method` <- paste0(path, "\\methods\\")

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

# NOTE: These are all vial configs

#' @export
qconfigMetabolomicsVanquishVialXCaliburSII_pos <- function(x, howOften) {
  .metabolomicsQueueVial(x, howOften = howOften, polarities = c("pos"), standard = "108mix")
}

#' @export
qconfigMetabolomicsVanquishVialXCaliburSII_neg <- function(x, howOften) {
  .metabolomicsQueueVial(x, howOften = howOften, polarities = c("neg"), standard = "108mix")
}

#' @export
qconfigMetabolomicsVanquishVialXCaliburSII_pos_neg <- function(x, howOften) {
  .metabolomicsQueueVial(x, howOften = howOften, polarities = c("pos", "neg"), standard = "108mix")
}

#' @export
qconfigLipidomicsVanquishVialXCaliburSII_pos <- function(x, howOften) {
  .metabolomicsQueueVial(x, howOften = howOften, polarities = c("pos"), standard = "EquiSPLASH")
}

#' @export
qconfigLipidomicsVanquishVialXCaliburSII_neg <- function(x, howOften) {
  .metabolomicsQueueVial(x, howOften = howOften, polarities = c("neg"), standard = "EquiSPLASH")
}

#' @export
qconfigLipidomicsVanquishVialXCaliburSII_pos_neg <- function(x, howOften) {
  .metabolomicsQueueVial(x, howOften = howOften, polarities = c("pos", "neg"), standard = "EquiSPLASH")
}

# NOTE: These are all plate configs

# TODO to be implemented



#' One blank sample
#'
#' @param x \code{data.frame} contains sample information, e.g., "Sample Name", "Sample ID", "Tube ID", "File Name", "Path", "Position"
#' @param plateId plate of the sample.
#'
#' @author Christian Panse <cp@fgcz.ethz.ch>, 2025-08-13
#' @returns \code{data.frame}
#' @export
#'
#' @examples
.blankMetabolomics <- function(x) {
  .createMetabolomicsSample(x, "blank")
}


.metabolomicsSampleConfig <- data.frame(
  sample_type = c("blank", "EquiSPLASH", "108mix_AA", "108mix_OAP", "pooledQC",
                  "pooledQCDil1", "pooledQCDil2", "pooledQCDil3",
                  "pooledQCDil4", "pooledQCDil5", "pooledQCDil6"),
  plateId = c("Y", "Y", "Y", "Y", "Y", "Y", "Y", "Y", "Y", "Y", "Y"),
  row = c("F", "F", "F", "E", "F", "H", "H", "H", "H", "H", "H"),
  col = c(1, 9, 9, 9, 8, 2, 3, 4, 5, 6, 7),
  file_name_template = c("{date}_{run}_C{container}_blank",
                         "{date}_{run}_C{container}_EquiSPLASH",
                         "{date}_{run}_C{container}_108mix_AA",
                         "{date}_{run}_C{container}_108mix_OAP",
                         "{date}_{run}_C{container}_pooledQC",
                         "{date}_{run}_C{container}_pooledQCDil1",
                         "{date}_{run}_C{container}_pooledQCDil2",
                         "{date}_{run}_C{container}_pooledQCDil3",
                         "{date}_{run}_C{container}_pooledQCDil4",
                         "{date}_{run}_C{container}_pooledQCDil5",
                         "{date}_{run}_C{container}_pooledQCDil6"),
  sample_name = c("blank", "EquiSPLASH", "108mix_AA", "108mix_OAP", "blank",
                  "QC dil1", "QC dil2", "QC dil3", "QC dil4", "QC dil5", "QC dil6"),
  stringsAsFactors = FALSE
)

#' Generic builder to create metabolomics sample rows from config
#' @param x input data frame with column structure
#' @param sample_types vector of sample type names to create
.createMetabolomicsSample <- function(x, sample_types) {
  # Look up configs for requested sample types
  configs <- .metabolomicsSampleConfig[
    .metabolomicsSampleConfig$sample_type %in% sample_types, ,
    drop = FALSE
  ]

  if (nrow(configs) == 0) {
    stop("No configurations found for sample types: ", paste(sample_types, collapse = ", "))
  }

  # Create output data frame with correct number of rows
  n_samples <- nrow(configs)
  result <- data.frame(matrix(NA, ncol = ncol(x), nrow = n_samples))
  colnames(result) <- colnames(x)

  # Fill in values from config for each sample
  for (i in 1:n_samples) {
    result$`File Name`[i] <- configs$file_name_template[i]
    result$Position[i] <- sprintf("%s:%s%d",
                                  configs$plateId[i],
                                  configs$row[i],
                                  configs$col[i])
    result$`Sample Name`[i] <- configs$sample_name[i]
    result$`Inj Vol`[i] <- x[['Inj Vol']][1]
  }

  result
}


.standardMetabolomics <- function(x, standard = "EquiSPLASH") {
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

.metabolomicsBlockPooledQCDilution <- function(x) {
  dilution_types <- paste0("pooledQCDil", 1:6)
  .createMetabolomicsSample(x, dilution_types)
}

.metabolomicsBlockStandardPoolQC <- function(x, standard) {
  blank <- .blankMetabolomics(x)
  std <- .standardMetabolomics(x, standard = standard)
  pooledQC <- .pooledQCMetabolomics(x)
  rbind(blank, std, pooledQC)
}

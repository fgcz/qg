# Proteomics ========================================
## 2024-07-04 Claudia Fortes / Christian Panse

#' @title queue config for Proteomics EVOSEP 6 x 12 x 8 Plate Hystar
#' @inheritParams qconfigMetabolomicsVanquishPlateXCaliburSII
#' @details increments clean and qc positions 
#' @author Claudia Fortes & Christian Panse 2024-07-04, 2025-01-29
#' @return a data.frame with the eight columns queue configuration
#' @details
#' The function is used to generate a queue configuration for the Proteomics
#' EVOSEP 6 x 12 x 8 Plate Hystar.
#' Here the clean and autoQC runs are taken from an incrementing position.
#' 
#' 
#' @export
qconfigProteomicsEVOSEP6x12x8PlateHystar <- function(x, howOften = 48,  ...){
  
  ## as a function of howOften
  howOftenClean <- as.integer(round(0.5 * howOften))
  
  stopifnot(is.integer(howOftenClean))
  message(paste0("howOftenClean:\t", howOftenClean))
  message(paste0("howOften qconfigProteomicsEVOSEP6x12x8PlateHystar:\t", howOften))
   
  ## fix column names and order
  c("File Name", "Path", "Position", "Inj Vol", "L3 Laboratory", "Sample ID", "Sample Name", "Instrument Method") -> cn
  stopifnot(all(cn %in% colnames(x)))  
  x[, cn] -> df
  df$`File Name` |> stringr::str_replace(pattern = "#", replacement = "_") -> df$`File Name` 
  
  Y <- c('A', 'B', 'C', 'D', 'E', 'F', 'G', 'H')

  output <- data.frame(matrix(ncol = 8, nrow = 0))
  colnames(output) <- cn
  
  clean <- data.frame(matrix(ncol = 8, nrow = 0))
  cleanAutoQC03 <- data.frame(matrix(ncol = 8, nrow = 0))
  
  colnames(df) <- cn
  
  colnames(clean) <- cn
  clean <- c("Clean", df$Path[1], "5:X:X", 1, "FGCZ", "clean", "clean", "clean")
  cleancount <- 2
  cleancountx <- 2
  cleancounty <- 1
  
  colnames(cleanAutoQC03) <- cn
  autoQC03count <- 2
  autoQC03countx <- 2
  autoQC03county <- 1
  
  ## ADD QC/CLEAN INBETWEEN =================================
  for (i in 1:nrow(df)){
    output <- rbind(output, df[i, ])
    
    if (i %% howOftenClean == 0) {
      clean <- c(sprintf("{date}_{run}_clean_%02d", cleancount),
                 df$Path[1], sprintf("5:%s,%d", Y[cleancounty], cleancountx),
                 1, "FGCZ", "clean", "clean", "clean")
      cleancountx <- cleancountx + 1
      cleancount <- cleancount + 1
      output <- rbind(output, clean)
    }
  
    if(i %% howOften == 0) {
      autoQC03 <- c(sprintf("{date}_{run}_autoQC03dia_%02d", autoQC03countx),
                    df$Path[1],
                    sprintf("6:%s,%d", Y[autoQC03county], autoQC03countx),
                    1, "FGCZ", "autoQC03", "autoQC03", "autoQC03")

      autoQC03countx <- autoQC03countx + 1
      autoQC03count <- autoQC03count + 1
      output <- rbind(output, autoQC03)
    } 
    

    if (cleancountx > 12){
      cleancountx <- 1
      cleancounty <- cleancounty + 1
    }
    
    if (autoQC03countx > 12){
      autoQC03countx <- 1
      autoQC03county <- autoQC03county + 1
    }
    
  } # for loop
  message("DEBUG -> howOften qconfigProteomicsEVOSEP6x12x8PlateHystar:\t")
 
  ## START ##########################
  autoQC03 <- c(sprintf("{date}_{run}_autoQC03dia_%02d", 1),
                df$Path[1],
                sprintf("6:%s,%d", Y[autoQC03county], 1),
                1, "FGCZ", "autoQC03", "autoQC03", "autoQC03")

    output <- rbind(autoQC03, output)

  clean <- c(sprintf("{date}_{run}_clean_%02d", 1), df$Path[1],
             sprintf("5:%s,%d", Y[1], 1),
             1, "FGCZ", "clean", "clean", "clean")
  
  output <- rbind(clean, output)
  cleancountx <- cleancountx + 1
  autoQC03countx <- autoQC03countx + 1
  if (cleancountx > 12){
    cleancountx <- 1
    cleancounty <- cleancounty + 1
  }
  
  if (autoQC03countx > 12){
    autoQC03countx <- 1
    autoQC03county <- autoQC03county + 1
  }
  
  ## TODO add autoQC03
  ## END ###############################
  clean <- c(sprintf("{date}_{run}_clean_%02d", cleancount), df$Path[1],
             sprintf("5:%s,%d", Y[cleancounty], cleancountx), 1, "FGCZ", "clean", "clean", "clean")
  output <- rbind(output, clean)

  autoQC03 <- c(sprintf("{date}_{run}_autoQC03dia_%02d", autoQC03countx),
                df$Path[1], sprintf("6:%s,%d", Y[autoQC03county], autoQC03countx),
                1, "FGCZ", "autoQC03", "autoQC03", "autoQC03")
  output <- rbind(output, autoQC03)
  
  message("qconfigProteomicsEVOSEP6x12x8PlateHystar  DONE")
  output  |> validate.qconfigProteomicsEVOSEP6x12x8PlateHystar()
}

validate.qconfigProteomicsEVOSEP6x12x8PlateHystar <- function(x){
  # Validate that x is a data.frame
  stopifnot(is.data.frame(x))
  
  # Validate column names
  required_columns <- c("File Name", "Path", "Position", "Inj Vol", 
                        "L3 Laboratory", "Sample ID", "Sample Name", "Instrument Method")
  missing_columns <- setdiff(required_columns, colnames(x))
  if(length(missing_columns) > 0) {
    stop(paste("The following required columns are missing:", paste(missing_columns, collapse = ", ")))
  }
  
  
  x
}



#' autoQC01 template
#'
#' @param x 
#' @param plateId 
#' @param QCrow 
#' @param mode 
#' @param containerid 
#' @param lssystem 
#'
#' @return \code{data.frame} object
.autoQC01 <- function(x, plateId = "1", QCrow = "H", mode = "", lc = "M_CLASS48_48"){
  message(paste0("autoQC01 lc = ", lc))
  data.frame(matrix(NA, ncol = ncol(x), nrow = 1)) -> pool
  colnames(pool) <- colnames(x)

  pool[1, "File Name"] <- sprintf("{date}_{run}_C{container}_autoQC01%s", mode)
  pool[1, "Position"] <- switch(lc,
                             "M_CLASS48_48" = "1:F,8",
                             "Vanquish"     = "1:F8",
                             sprintf("%s:%s%d", plateId, QCrow, 1)
  )
  pool[1, "Sample Name"] <- sprintf("autoQC01%s", mode)

  pool$`Inj Vol` <- 2
  pool
}

#' @title queue confiug for Proteomics 
#' @details increments clean and qc positions 
#' \itemize{
#' \item position mapping f: 1-48 -> (1:8; A:F) starting: A,1; A,2; ...; A,8; B,1; ...; F,8
#' }
#' @author Christian Panse 2024-09-03
#' 
#' qconfig metabolomics for plates
#'
#' @inheritParams qconfigMetabolomicsVanquishPlateXCaliburSII
#' @export
qconfigProteomicsVialXCaliburLCDevices <- function(x, howOften = 4, ...){
  cn <- c("File Name", "Path", "Position", "Inj Vol", "L3 Laboratory",
          "Sample ID", "Sample Name", "Instrument Method")
  
  # base::save(x, file="/tmp/mx.RData")
  # browser()
  # ignore F (last) row TODO(cp): how to generalize it?
  x[grepl(pattern = ":[ABCDEFG][,]{0,1}[1-9]", x = x$Position), ] -> x
  
  im <- paste0(x$Path[1], "\\methods\\")
  
  x |> .insertSample(howOften = howOften, sampleFUN = .autoQC01, path = x$Path[1], ...) -> x
 
  # START
  x |> .insertSample(where = 0, sampleFUN = .autoQC01, path = x$Path[1], ...) -> x
  
  # END
  x |> .insertSample(where = (nrow(x) + 1), sampleFUN = .autoQC01, path = x$Path[1], ...) -> x

  x$`L3 Laboratory` <- "FGCZ"
  # x$Position |> sapply(FUN = .parsePlateNumber) -> x$Position
  x$`Instrument Method` <- im
  # x$Position |> sapply(FUN = .parsePlateNumber) -> x$Position
  x[, cn]
}

#' @title queue config for Proteomics Vial and Chronos 
#' @inherit qconfigMetabolomicsVanquishPlateXCaliburSII
#' @export
qconfigProteomicsVialChronos <- function(x, howOften = 4, ...){
  # qconfigProteomicsVialXCalibur(x, howOften = howOften, ...) -> rv
  
  c("Analysis Method",
    "Source Tray",
    "Source Vial",
    "Sample Name",
    "Xcalibur Method",
    "Xcalibur Filename",
    "Xcalibur Post Acquisition Program",
    "Xcalibur Output Dir",
    "Comment") -> hh         
  
  
  x |> .insertSample(howOften = howOften, sampleFUN = .autoQC01, path = x$Path[1], ...) -> x
  # START
  x |> .insertSample(where = 0, sampleFUN = .autoQC01, path = x$Path[1], ...) -> x
  
  # END
  x |> .insertSample(where = (nrow(x) + 1), sampleFUN = .autoQC01, path = x$Path[1], ...) -> x
  
  
  data.frame(
    `Analysis Method` = rep("C:\\*cam", length = nrow(x)),
    `Source Tray` = rep("EvoSlot 1", length = nrow(x)),
    `Source Vial` = 1:nrow(x),
    `Sample Name` = rep("", nrow(x)),
    `Xcalibur Method` = rep("", nrow(x)),
    `Xcalibur Filename` = x$`File Name`,
    `Xcalibur Post Acquisition Program` = rep("c:\\FGCZ\\BioBeamer\\biobeamer.bat", nrow(x)),
    `Xcalibur Output Dir` = x$Path,
    `Comment` = rep("", nrow(x))
  ) -> x
  
  colnames(x) <- hh
  x
}



#'  Proteomics Exploris EVOSEP 6x12x8 Plate Chronos
#' @inheritParams qconfigMetabolomicsVanquishPlateXCaliburSII
#' @returns a data.frame object
#' @author Antje Dittmann 2024-09-03 2025-01-31
#' @export
qconfigProteomicsPlateChronos <- function(x, howOften = 4, ...){
  c("Analysis Method",
    "Source Tray",
    "Source Vial",
    "Sample Name",
    "Xcalibur Method",
    "Xcalibur Filename",
    "Xcalibur Post Acquisition Program",
    "Xcalibur Output Dir",
    "Comment") -> hh        
  
  
  counterAutoQC01 <<- 1
  counterAutoQC03 <<- 49
  counterClean <<-  1
  x |> .insertSample(howOften = howOften, modOffset = -1, sampleFUN = .chronos_autoQC01, path = x$Path[1], ...) -> x
  
  
  # START
  ## x |> .insertSample(where = 0, sampleFUN = .chronos_autoQC01, path = x$Path[1], ...) -> x
  
  # END
  x |> .insertSample(where = (nrow(x) + 1), sampleFUN = .chronos_autoQC01, path = x$Path[1], ...) -> x
  x |> .insertSample(where = (nrow(x) + 1), sampleFUN = .chronos_autoQC03, path = x$Path[1], ...) -> x
  
  
  data.frame(
    `Analysis Method` = rep("", length = nrow(x)),
    `Source Tray` = x$Tray,
    `Source Vial` = x$Position,
    `Sample Name` = x$`Sample Name`,
    `Xcalibur Method` = "", ## x$`Instrument Method`,
    `Xcalibur Filename` = x$`File Name`,
    `Xcalibur Post Acquisition Program` = rep("c:\\FGCZ\\BioBeamer\\biobeamer.bat", nrow(x)),
    `Xcalibur Output Dir` = x$Path,
    `Comment` = rep("", nrow(x))
  ) -> x
  
  colnames(x) <- hh
  x
  
}

## Clean Tray is Tray 6
## AutoQC01 is Tray 5 1-48
## AutoQC03 is Tray 5 49-96
#' @noRd
.chronos_autoQC01 <- function(x, ...){
  data.frame(matrix(NA, ncol = ncol(x), nrow = 2)) -> pool
  colnames(pool) <- colnames(x)


  ## 1st the clean
  pool$`Position`[1] <- counterClean;

  pool$`Tray`[1] <- 6
  pool$`Sample Name`[1] <- sprintf("clean")
  pool$`File Name`[1] <- sprintf("{date}_{run}_C{container}_clean")

  # 2nd the autoQC01
  pool$`Position`[2] <- counterAutoQC01;
  pool$`Tray`[2] <- 5
  pool$`Sample Name`[2] <- sprintf("autoQC01")
  pool$`File Name`[2] <- sprintf("{date}_{run}_C{container}_autoQC01")

  ## TODO(cpanse): think about the counters
  counterAutoQC01 <<- counterAutoQC01 + 1
  counterClean <<- counterClean + 1
  pool
}

## Clean Tray is Tray 6
## AutoQC01 is Tray 5 1-48
## AutoQC03 is Tray 5 49-96
#' @noRd
.chronos_autoQC03 <- function(x, ...){
  data.frame(matrix(NA, ncol = ncol(x), nrow = 1)) -> pool
  colnames(pool) <- colnames(x)


  pool$`Position`[1] <- counterAutoQC03;
  pool$`Tray`[1] <- 5
  pool$`Sample Name`[1] <- sprintf("autoQC03")
  pool$`File Name`[1] <- sprintf("{date}_{run}_C{container}_autoQC03")

  ## TODO(cpanse): think about the counters
  counterAutoQC03 <<- counterAutoQC03 + 1

  pool
}


qconfigProteomicsPlateChronosX <- function(x, howOften = 4, ...){
  shiny::showNotification("qconfigProteomicsPlateChronosX is not implemented.",
                          duration = 20, type = "error")
  return(x)
}

qconfigProteomicsPlateXCaliburSII <- function(x, ...){
  shiny::showNotification("method qconfigProteomicsPlateXCaliburSII is not implemented.",
                          duration = 20, type = "error")
 return(x)
}
  
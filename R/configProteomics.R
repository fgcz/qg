# Proteomics ========================================
## 2024-07-04 Claudia Fortes / Christian Panse

#' @title queue confiug for Proteomics EVOSEP 6x12x8 Plate Hystar
#' @inheritParams qconfigMetabolomicsVanquishPlateXCalibur
#' @details increments clean and qc positions 
#' @author Claudia Fortes & Christian Panse
#' @export
qconfigProteomicsEVOSEP6x12x8PlateHystar <- function(x, howOften = 48,  ...){
  
  ## as a function of howOften
  howOftenClean <- as.integer(round(0.5 * howOften))
  
  stopifnot(is.integer(howOftenClean))
  message(paste0("howOftenClean:\t", howOftenClean))
  message(paste0("howOften:\t", howOften))
  
  df <- x
  Y <- c('A', 'B', 'C', 'D', 'E', 'F', 'G', 'H')
  
  currentdate <- format(Sys.time(), "%Y%m%d")
  output <- data.frame(matrix(ncol = 8, nrow = 0))
  colnames(output) <- c("File Name", "Path", "Position", "Inj Vol", "L3 Laboratory", "Sample ID", "Sample Name", "Instrument Method")
  
  clean <- data.frame(matrix(ncol = 8, nrow = 0))
  cleanAutoQC03 <- data.frame(matrix(ncol = 8, nrow = 0))
  
  colnames(df) <- c("File Name", "Path", "Position", "Inj Vol", "L3 Laboratory", "Sample ID", "Sample Name", "Instrument Method")
  
  colnames(clean) <- c("File Name", "Path", "Position", "Inj Vol", "L3 Laboratory", "Sample ID", "Sample Name", "Instrument Method")
  clean <- c("Clean", df$Path[1], "5:X:X", 1, "FGCZ", "clean", "clean", "clean")
  cleancount <- 1
  cleancountx <- 1
  cleancounty <- 1
  
  colnames(cleanAutoQC03) <- c("File Name", "Path", "Position", "Inj Vol", "L3 Laboratory", "Sample ID", "Sample Name", "Instrument Method")
  autoQC03count <- 1
  autoQC03countx <- 1
  autoQC03county <- 1
  
  ## ADD QC/CLEAN INBETWEEN =================================
  for (i in 1:nrow(df)){
    output <- rbind(output, df[i, ])
    
    
    if (i %% howOftenClean == 0) {
      clean <- c(sprintf("%s_@@@_clean_%02d", currentdate, cleancount), df$Path[1], sprintf("5:%s,%d", Y[cleancounty], cleancountx), 1, "FGCZ", "clean", "clean", "clean")
      cleancountx <- cleancountx + 1
      cleancount <- cleancount + 1
      output <- rbind(output, clean)
    }
  
    if(i %% howOften == 0) {
      #clean <- c(sprintf("%s_@@@_clean_%02d", currentdate, cleancount), df$Path[1], sprintf("5:%s,%d", Y[cleancounty], cleancountx), 1, "FGCZ", "clean", "clean", "clean")
      #cleancountx <- cleancountx + 1
      #cleancount <- cleancount + 1
      #output <- rbind(output, clean)
      
      autoQC03 <- c(sprintf("%s_@@@_autoQC03dia_%02d", currentdate, autoQC03countx), df$Path[1], sprintf("6:%s,%d", Y[autoQC03county], autoQC03countx), 1, "FGCZ", "autoQC03", "autoQC03", "autoQC03")
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
  
  ## START
  autoQC03 <- c(sprintf("%s_@@@_autoQC03dia_00", currentdate, autoQC03countx), df$Path[1], sprintf("6:%s,%d", Y[autoQC03county], autoQC03countx), 1, "FGCZ", "autoQC03", "autoQC03", "autoQC03")
  output <- rbind(autoQC03, output)
  clean <- c(sprintf("%s_@@@_clean_00", currentdate, cleancount), df$Path[1], sprintf("5:%s,%d", Y[cleancounty], cleancountx), 1, "FGCZ", "clean", "clean", "clean")
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
  ## END
  clean <- c(sprintf("%s_@@@_clean_%02d", currentdate, cleancount), df$Path[1], sprintf("5:%s,%d", Y[cleancounty], cleancountx), 1, "FGCZ", "clean", "clean", "clean")
  output <- rbind(output, clean)
  
  autoQC03 <- c(sprintf("%s_@@@_autoQC03dia_ZZ", currentdate, autoQC03countx),
                df$Path[1], sprintf("6:%s,%d", Y[autoQC03county], autoQC03countx), 1, "FGCZ", "autoQC03", "autoQC03", "autoQC03")
  output <- rbind(output, autoQC03)
  
  output 
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
.autoQC01 <- function(x, plateId = "1", QCrow = "H", mode = "", containerid="", lc = "M_CLASS48_48"){
  data.frame(matrix(NA, ncol = ncol(x), nrow = 1)) -> pool
  colnames(pool) <- colnames(x)
  currentdate <- format(Sys.time(), "%Y%m%d")
  
  pool[1, "File Name"] <- sprintf("%s_@@@_C%s_autoQC01%s", currentdate, containerid, mode)
  
  if (lc == "M_CLASS48_48"){
    pool$Position[1] <- "1:F,8"
  }else{
    pool$Position[1] <- sprintf("%s:%s%d", plateId, QCrow, 1)
  }
  
  pool$`Sample Name`[1] <- sprintf("autoQC01%s", mode)
  
  pool$`Inj Vol` <- 2
  pool
}

#' @title queue confiug for Proteomics 
#' @inheritParams qconfigMetabolomicsVanquishPlateXCalibur
#' @details increments clean and qc positions 
#' \itemize{
#' \item position mapping f: 1-48 -> (1:8; A:F) starting: A,1; A,2; ...; A,8; B,1; ...; F,8
#' }
#' @author Christian Panse 2024-09-03
#' 
#' qconfig metabolomics for plates
#'
#' @inheritParams qconfigMetabolomicsVanquishPlateXCalibur
#' @export
qconfigProteomicsVialXCalibur <- function(x, howOften = 4, ...){
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
#' @inherit qconfigMetabolomicsVanquishPlateXCalibur
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
  ) -> df
  
  df
}
  
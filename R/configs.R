#R
## 2024-07-04 Clauda Fortes / Christian Panse


#' Generic function to place a sample into a queue configuration
#' \code{...} is used to pass the parameters to \code{sampleFUN} method.
#' 
#' @param x \code{data.frame} contains sample information, e.g., \code{"Sample Name", "Sample ID", "Tube ID", "File Name", "Path", "Position"}.
#'
#' @param where the position where the sample should be inserted
#' @param howOften how frequently the sample should be inserted
#' @param sampleFUN the implemented function used defining the sample, e.g., \link[qg]{.autoQC01VialXCaliburSII}.
#' @param path the path of the sample.
#' @param ... parameters to pass to sampleFUN function.
#' 
#' @aliases Christian.Panse <cp@fgcz.ethz.ch> 2023
#'
#' @export
.insertSample <- function(x,
                         modOffset = 0,
                         where = NA,
                         howOften = round(nrow(x) / 2),
                         sampleFUN = NA,
                         path = NA, ...){
  output <- data.frame(matrix(ncol = ncol(x), nrow = 0))
  colnames(output) <- colnames(x)
  
  if (is.na(where)){
    for (i in 1:nrow(x)){
      if (((i + modOffset) %% howOften) == 0){
        ## inject sampleFUN
        plateId <- output$Position[nrow(output)] |> substr(1,1)
        rbind(output, sampleFUN(x, plateId = plateId, ...)) -> output
      }
      rbind(output, x[i, ]) -> output
    }
  }else if (where == 0){
    plateId <- x$Position[1] |> substr(1,1)
    rbind(sampleFUN(x, plateId = plateId, ...), x) ->  output
  }else if (where > nrow(x)){
    plateId <- x$Position[nrow(x)] |> substr(1,1)
    rbind(x, sampleFUN(x, plateId = plateId, ...)) ->  output
  }else{stop("Invalid arguments")}
  
  output$Path <- path
  output
}



## TODO(cp): check replacement for QHF2!!
#' generator Hystar config XML file
#'
#' @param x a data frame
#' @param file output
#' @import XML
#' @author Tobias Kockmann (intial), Claudia Fortes, Christian Panse
#'
#' @return xml file
#' @export
.toHystar <- function(x,
                      file='file.xml'){
	data.frame(
	    Position = x$Position |>
	      stringr::str_replace("^", "S") |>
	      stringr::str_replace(":", "-") |>
	      stringr::str_replace(",", ""),
	    SampleID = sprintf("%s", x$"File Name"),
	    SampleComment = "",
	    Volume = 1,
	    DataPath = x$Path |> stringr::str_replace("QEXACTIVEHF_2", "TIMSTOFFLEX_1"),
	    SuperMethod = "",
	    ResultDatafile = sprintf("%s\\%s.d", x$Path, x$"File Name"),
	    ACQEND_EXECUTE = "C:\\FGCZ\\Biobeamer\\biobeamer.bat"
	    ) -> df


    xml <- XML::xmlTree()
    xml$addTag("SampleTable")
    dump <- lapply(1:nrow(df), FUN = function(i) xml$addTag("Sample", close=TRUE, attrs=df[i, ]))
      
    message(paste0("Saving XML to file '", file, "' ..."))
    rvSave <- XML::saveXML(xml$value(), file = file, encoding = "utf-8")
}

#' Interpolates placeholders in filenames.
#' @param x
#'
#' @export
.interpolateFilenames <- function(x, container) {
  column <- ifelse(
    "File Name" %in% colnames(x),
    "File Name",
    "Xcalibur Filename"
  )
  date <- format(Sys.time(), "%Y%m%d")

  # Interpolate Path (same for all rows, no run counter needed)
  if ("Path" %in% colnames(x) && length(unique(x$Path)) == 1) {
    path_template <- x$Path[1]
    interpolated_path <- stringr::str_glue(
      path_template,
      date = date,
      container = container
    )
    x$Path <- rep(interpolated_path, nrow(x))
  }

  # Interpolate Instrument Method (can reference {path})
  if ("Instrument Method" %in% colnames(x) && length(unique(x$`Instrument Method`)) == 1) {
    method_template <- x$`Instrument Method`[1]
    x$`Instrument Method` <- rep(
      stringr::str_glue(
        method_template,
        path = x$Path[1]
      ),
      nrow(x)
    )
  }

  # Interpolate File Name (with run counter)
  for (i in seq_len(nrow(x))) {
    # TODO kept until {run} is used everywhere (replace @@@)
    template_str <- stringr::str_replace_all(x[[column]][[i]], "@@@", "{run}")
    # TODO also unclear why it's here
    template_str <- stringr::str_replace(template_str, "#", "_")

    # NOTE: Only order-level placeholders are supported here.
    #       Do not add too many placeholders to avoid confusion.
    x[[column]][[i]] <- stringr::str_glue(
      template_str,
      date = date,
      run = sprintf("%03d", i),
      container = container
    )
  }

  # TODO will these work for XCalibur?
  stopifnot(
    vapply(x$`File Name`, qg:::.validateFilename, FUN.VALUE = TRUE) |> all()
  )
  x
}

#' Read samples of a given container
#' @author Christian Panse
#' @param containerID integer container ID
#' @param login character login
#' @param webservicepassword character webservice password you get from your bfabric user profile page
#' @param posturl character bfabric REST post url
#' @importFrom bfabricShiny read uploadResource
#' @export
#' @examples
#' if (all(c('login', 'webservicepassword', 'bfabricposturl') %in% names(Sys.getenv()))){
#'   qg::.readSampleOfContainer(
#'     containerID = 35464,
#'     login = Sys.getenv('login'), 
#'     webservicepassword = Sys.getenv('webservicepassword'), 
#'     posturl = Sys.getenv('bfabricposturl')
#'     )
#' }
.readSampleOfContainer <- function(containerID, login, webservicepassword, posturl){
  res <- bfabricShiny::read(login, webservicepassword, posturl = posturl,
                            endpoint = "sample",
                            maxitems = 2000,
                            query = list('containerid' = containerID))$res
  
  data.frame('Sample Name' = sapply(res, function(x)x$name),
             `Sample ID`= sapply(res, function(x)x$id),
             "Tube ID" = sapply(res, function(x)x$tubeid),
             stringsAsFactors = FALSE) -> df
  
  colnames(df) <- c("Sample Name", "Sample ID", "Tube ID")
  
  df[order(df$`Sample ID`), ]
}



#' @importFrom stringr str_replace
.extractSampleIdfromTubeID <- function(containerid, tid){
  sapply(tid, FUN = function(x){
    pattern = sprintf("%s/[0-9]+", containerid)
    if(grepl(pattern, x)){
      x |> stringr::str_replace("/", "-")
    }else{
      containerid
    }
  })
}


validate.composePlateSampleTable <- function(x){
  #stopifnot(ncol(x) == 10 | ncol(x) == 8)
  message("Validating composePlateSampleTable ncol = ", ncol(x))
  
  stopifnot(all(c("Sample ID",
                  "Sample Name",
                  "Tube ID",
                  "Position",
                  "GridPosition",
                  "File Name",
                  "Path",
                  "Inj Vol",
                  "L3 Laboratory",
                  "Instrument Method") %in% colnames(x)))
  x
}
#' Compose plate sample table
#'
#' @param p 
#' @param orderID 
#' @param area 
#' @param instrument 
#' @param user 
#' @param injVol 
#' @param plateCounter 
#' @param randomization 
#'
#' @export
.composePlateSampleTable <- function(p,
                                     orderID = 34843,
                                     area = "Metabolomics",
                                     instrument = 'ASTRAL_1',
                                     system = NULL,
                                     lc = "M_CLASS48_48",
                                     user = 'cpanse',
                                     injVol = 3.5, 
                                     plateCounter = 0,
                                     randomization = 'plate'){
  # TODO this looks a bit weird to me
  p$"File Name" <- sprintf("{date}_{run}_C{container}_S%d_%s",
                           .extractSampleIdfromTubeID(orderID, p$`Tube ID`),
                           p$"Sample ID",
                           p$"Sample Name")

  p$"Path" <- sprintf("D:\\Data2San\\p{container}\\%s\\%s\\%s_{date}",
                     area, instrument, user)
  
  ## TODO(cpanse): test it
  ## TODO(cpanse): generalise Position and GridPosition
  if (lc == "Vanquish"){
    ## changed from Position to GridPosition 2024-12-12 CP
    VanquishPlateIDs <- c("Y", "R", "B", "G")
    p$Position <- sprintf("%s:%s", VanquishPlateIDs[plateCounter], p$GridPosition)
  } else if (system == "Chronos" && lc == "EVOSEP6x12x8"){
    message("Chronos EVOSEP6x12x8 .composePlateSampleTable")
    p$Position <- sprintf("%s", p$Position)
    p$Tray <- sprintf("%d", rep(plateCounter, nrow(p)))
  }else {
    p$Position <- sprintf("%d:%s", plateCounter, p$GridPosition)
  }
  
  p$"Inj Vol" <- injVol
  
  p$"L3 Laboratory" <- "FGCZ"
  
  p$"Instrument Method" <- sprintf("%s\\methods\\", p$Path)
  
  if (randomization == "plate"){
    set.seed(872436)
    p[sample(nrow(p)), ] -> p
  }
  
  plateCounter <<- plateCounter + 1
  ## TODO(cp): check if this is necessary
  #p[, columnOrder]
  
  
  validate.composePlateSampleTable(p)
}


#' Compose vial sample table
#'
#' @inherit .composePlateSampleTable
#'
#' @export
#' @examples
#'  if (all(c('login', 'webservicepassword', 'bfabricposturl') %in% names(Sys.getenv()))){
#' .readSampleOfContainer(36104,
#'     Sys.getenv('login'),
#'     Sys.getenv('webservicepassword'),
#'     Sys.getenv('bfabricposturl')) |>
#'   .composeVialSampleTable(orderID = 34843,
#'       lc = "M_CLASS48_48",
#'       randomization = FALSE) -> x
#'   x |> head()
#'   }
#'   
#'  if (all(c('login', 'webservicepassword', 'bfabricposturl') %in% names(Sys.getenv()))){
#' .readSampleOfContainer(34843,
#'     Sys.getenv('login'),
#'     Sys.getenv('webservicepassword'),
#'     Sys.getenv('bfabricposturl')) |>
#'   .composeVialSampleTable(orderID = 34843,
#'       randomization = TRUE,
#'       lc = "Vanquish") -> x
#'       x |> head()
#'       
#' # x|> qg:::qconfigMetabolomics()|> .replaceRunIds() -> xx
#' }
.composeVialSampleTable <- function(x, orderID = 34843,
                                area = "Metabolomics",
                                instrument = 'ASTRAL_1',
                                user = 'cpanse',
                                injVol = 3.5, 
                                lc = 'M_CLASS48_48',
                                randomization = TRUE){
  
  p <- x
  p$"File Name" <- sprintf("{date}_{run}_C{container}_S%d_%s",
                           p$"Sample ID",
                           p$"Sample Name")
  p$"Path" <- sprintf("D:\\Data2San\\p{container}\\%s\\%s\\%s_{date}",
                     area, instrument, user)
   
  if (lc == "M_CLASS48_48"){
    message("lc = M_CLASS48_48")
    .lcWaters(n = nrow(p)) -> p$Position
  }else{
    message("lc = 'Vanquish'")
    if (area == "Metabolomics"){
      ## Metabolomics uses the F row for QCs
      qg:::.lcVanquish(n = nrow(p), patternLastPos = "E9", availablePlates = c("Y", "R", "B", "G") ) |>
        sapply(FUN = .parseVanquishPlateNumber) -> p$Position
    }else if(area == "Proteomics") {
      ## Proteomics uses the Blue plate for QC
      qg:::.lcVanquish(n = nrow(p), patternLastPos = "F9", availablePlates = c("Y", "R", "G") ) |>
      sapply(FUN = .parseVanquishPlateNumber) -> p$Position
    }else{
      stop(sprintf("%s - is no valid LC system", lc))
    }
  }
  #browser()
  p$"Inj Vol" <- injVol
  p$"L3 Laboratory" <- "FGCZ"
  p$"Instrument Method" <- sprintf("%s\\methods\\", p$Path)
  
  if (randomization){
    split(1:nrow(p), substr(p$Position, 1, 1)) |>
      lapply(function(idx){p[idx[sample(length(idx))], ]}) |>
      Reduce(f = rbind) -> p
  }
  #browser()
   p
}

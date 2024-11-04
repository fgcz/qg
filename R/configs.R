#R
## 2024-07-04 Clauda Fortes / Christian Panse


#' Generic function to place a sample into a queue configuration
#' \code{...} is used to pass the parameters to FUN
#' @param x 
#'
#' @param where the position where the sample should be inserted
#' @param howOften how frequently the sample should be inserted
#' @param sampleFUN the function to generate the sample
#' @param path the path of the sample
#' @param ... parameters to pass to sampleFUN
#'
#' @export
.insertSample <- function(x,
                         where = NA,
                         howOften = round(nrow(x) / 2),
                         sampleFUN = NA,
                         path=NA, ...){
  output <- data.frame(matrix(ncol = ncol(x), nrow = 0))
  colnames(output) <- colnames(x)
  
  if (is.na(where)){
    for (i in 1:nrow(x)){
      if ((i %% howOften) == 0){
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



## TODO(cp): check replacemet for QHF2!!
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

#' Replace run IDs in a data frame
#' @param x 
#'
#' @export
.replaceRunIds <- function(x){

  cn <- NULL
  
  for (cnc in c("File Name", "Xcalibur Filename")){
    if (cnc %in% colnames(x)){
      cn <- cnc
      message("Column name to replace @@@ run ids: ", cn)
      break
    }
  }
  
  shiny::validate(shiny::need(isFALSE(is.null(cn)),
                       "No column name to replace @@@ run ids."))
                  
	for (i in 1:nrow(x)){
		rn <- sprintf("_%03d_", i)
		x[[cn]][i] |>
		  stringr::str_replace("_@@@_", rn) -> x[[cn]][i]
		
		x[[cn]][i] |>
		  stringr::str_replace("#", "_") -> x[[cn]][i]

	}
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


#' compose plate sample table
#'
#' @param p 
#' @param orderID 
#' @param area 
#' @param mode 
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
                                     mode = "",
                                     instrument = 'ASTRAL_1',
                                     user = 'cpanse',
                                     injVol = 3.5, 
                                     plateCounter = 0,
                                     randomization = 'plate'){
  format(Sys.time(), "%Y%m%d") -> currentdate
  
  p$"File Name" <- sprintf("%s_@@@_C%s_S%d%s_%s",
                           currentdate,
                           .extractSampleIdfromTubeID(orderID, p$`Tube ID`),
                           p$"Sample ID",
                           mode,
                           p$"Sample Name")
  
  p$"Path" <- paste0("D:\\Data2San\\p", orderID, "\\", area,
                     "\\", instrument, "\\",
                     user, "_", currentdate)
  p$"Sample Name" <- paste0(p$"Sample Name", mode)
  
  p$Position <- sprintf("%d:%s", plateCounter, p$Position)
  
  p$"Inj Vol" <- injVol
  
  p$"L3 Laboratory" <- "FGCZ"
  
  p$"Instrument Method" <- sprintf("%s\\methods\\", p$Path)
  
  if (randomization == "plate"){
    set.seed(872436)
    p[sample(nrow(p)), ] -> p
  }
  
  plateCounter <<- plateCounter + 1
  p[, columnOrder]
}


#' compose vial sample table
#'
#' @param x 
#' @param orderID 
#' @param area 
#' @param mode 
#' @param instrument 
#' @param user 
#' @param injVol 
#' @param randomization 
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
                                mode = "",
                                instrument = 'ASTRAL_1',
                                user = 'cpanse',
                                injVol = 3.5, 
                                lc = 'M_CLASS48_48',
                                randomization = TRUE){
  
  format(Sys.time(), "%Y%m%d") -> currentdate
  p <- x
  p$"File Name" <- sprintf("%s_@@@_C%s_S%d%s_%s",
                           currentdate,
                           orderID,
                           p$"Sample ID",
                           mode,
                           p$"Sample Name")
  p$"Path" <- paste0("D:\\Data2San\\p", orderID, "\\", area,
                     "\\", instrument, "\\",
                     user, "_", currentdate)
  p$"Sample Name" <- paste0(p$"Sample Name", mode)
  
  if (lc == "M_CLASS48_48"){
    message("M_CLASS48_48")
    .lcWaters(n = nrow(p)) -> p$Position
  }else{
    message("Vanquish")
    .lcVanquish(n = nrow(p)) |> sapply(FUN = .parseVanquishPlateNumber) -> p$Position
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

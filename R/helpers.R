#R

## Christian Panse <cp@fgcz.ethz.ch> 2025-08-06

.validateReadConfigInstrument <- function(x){
  
  stopifnot(c("area", "system", "lc", "instrument") %in% names(x))
  
  stopifnot(x$area |> unique()  %in% c("Proteomics","Metabolomics"))
  
  stopifnot(x$system |> unique() %in% c("VialHystar",
                                        "PlateHystar",
              "VialChronos",
              "PlateChronos",
              "VialXCaliburSII",
              "VialXCaliburLCDevices",
              "VialXCaliburSII",
              "PlateXCaliburLCDevices",
              "PlateXCaliburSII"))
  
  stopifnot(x$lc |> unique() %in% c("EVOSEP6x12x8", "Vanquish", "M_CLASS48_48"))
  
  x
}

.readConfigInstrument <- function(f = file.path(system.file(package = "qg"), "extdata", 
                                             "instrument.csv") ){
  stopifnot(file.exists(f))
  
  message("Reading instrument configuration from file", f, "...")
  
  read.table(f, header = TRUE, sep = ";") |>
    .validateReadConfigInstrument()
}

#' export
.readPackageFile <- function(f = "test-c37530-MetabolomicsVanquishVialXCaliburSIIEquiSPLASH.RData"){
  
  file.path(system.file(package = "qg"), "extdata", f) -> f
  stopifnot(file.exists(f))
  
  e <- new.env()
  base::load(file = f, envir = e)
  
  e$df
}

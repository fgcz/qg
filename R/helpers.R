#R

## Christian Panse <cp@fgcz.ethz.ch> 2025-08-06

.validateReadInstrumentConfig <- function(x){
  x
}

.readInstrumentConfig <- function(){
  file.path(system.file(package = "qg"), "extdata", 
            "instrument.csv") -> f
  stopifnot(file.exists(f))
  read.table(f, header = TRUE, sep = ";") |>
    .validateReadInstrumentConfig()
}

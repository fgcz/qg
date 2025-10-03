#R

.onLoad <- function(libname, pkgname) {
  # Load metabolomics sample configuration from TSV on package load
  .metabolomicsSampleConfig <<- .readMetabolomicsSampleConfig()
}

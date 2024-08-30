#R


library(testthat)


stopifnot(all(c('login', 'webservicepassword', 'bfabricposturl') %in% names(Sys.getenv())))

Sys.getenv('login') -> login
Sys.getenv('webservicepassword') -> webservicepassword
Sys.getenv('bfabricposturl') -> posturl

suppressPackageStartupMessages(library(qg))

test_check("qg")


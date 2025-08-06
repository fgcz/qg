#R


library(testthat)


stopifnot(all(c('login', 'webservicepassword', 'bfabricposturl') %in% names(Sys.getenv())))

Sys.getenv('login') ->> login
Sys.getenv('webservicepassword') ->> webservicepassword
Sys.getenv('bfabricposturl') ->> posturl

message(paste0("login = ", login))

suppressPackageStartupMessages(library(qg))
suppressPackageStartupMessages(library(bfabricShiny))

#test_check("qg")


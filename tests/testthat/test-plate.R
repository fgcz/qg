#R

context("container")

test_that("test plate", {
 
  
  orderid <- 37202
  plateid <- c(4748, 4749)
  plateCounter <- 1
  plateid |>
    lapply(FUN = function(pid){
      bfabricShiny::readPlate(pid,
                login = login,
                webservicepassword =  webservicepassword,
                posturl = posturl) |>
        qg::.composePlateSampleTable(orderID = orderid,
                                     instrument = "TIMSTOFFLEX_1",
                                     system = "HyStar",
                                     lc = "Evosep",
                                     user = "cpanse",
                                     injVol = 1,
                                     area = "Proteomics",
                                     mode = "",
                                     plateCounter = plateCounter,
                                     randomization = FALSE) -> p
      plateCounter <<- plateCounter + 1
      p
    }) |> Reduce(f = rbind) -> df
  
  
  expect_true(ncol(df) == 10)
  expect_no_error(qg:::validate.composePlateSampleTable(df))
  expect_error(qg:::validate.composePlateSampleTable(df[, 1:9]))
  
})


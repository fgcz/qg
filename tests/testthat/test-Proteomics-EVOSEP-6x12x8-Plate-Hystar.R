#R

context("Proteomics c37202 TIMSTOFFLEX_1 EVOSEP 6x12x8 Plate Hystar")

test_that("test Proteomics TimsTOFFlex EVOSEP 6x12x8 Plate Hystar", {
 
  ####################################################
  orderid <- 37202
  plateid <- c(4748, 4749)
  plateCounter <- 1
  qFUN <- "qconfigProteomicsEVOSEP6x12x8PlateHystar"
  ####################################################
  
  plateid |>
    lapply(FUN = function(pid){
      bfabricShiny::readPlate(pid,
                login = login,
                webservicepassword =  webservicepassword,
                posturl = posturl) |>
        qg::.composePlateSampleTable(orderID = orderid,
                                     instrument = "TIMSTOFFLEX_1",
                                     system = "HyStar",
                                     lc = "EVOSEP",
                                     user = "cpanse",
                                     injVol = 1,
                                     area = "Proteomics",
                                     mode = "",
                                     plateCounter = plateCounter,
                                     randomization = FALSE) -> p
      plateCounter <<- plateCounter + 1
      p
    }) |> Reduce(f = rbind) -> df
  
  df -> df0
  
  expect_true(ncol(df) == 10)
  expect_no_error(qg:::validate.composePlateSampleTable(df0))
  
  expect_error(qg:::validate.composePlateSampleTable(df[, 1:9]))
  
  colnames(df) -> cn
  paste0(cn[1], "__") -> cn[1]
  
  colnames(df) <- cn
  expect_error(qg:::validate.composePlateSampleTable(df))
  
  
  
  
  do.call(what = qFUN,
          args = list(x = df0,
                      containerid = orderid[1],
                      howOften = as.integer(16))) -> df1
  
  expect_equal(df1$`Inj Vol` |> as.integer() |> sum() , nrow(df1))
  
  df1 |> qg::.replaceRunIds() -> df2
  
  
})


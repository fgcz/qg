
#R

context("Proteomics c28073 ASTRAL_1  Vanquish Vial XCalibur")

test_that("test Proteomics ASTRAL_1 XCalibur Vanquish Vial", {
  
  ####################################################
  orderId <- 28073
  instrumentMode <- ""
  qFUN <- "qconfigProteomicsVialXCaliburLCDevices"
  
  ####################################################
  
  rv <- qg::.readSampleOfContainer(containerID = orderId,
                                   webservicepassword = webservicepassword,
                                   posturl = posturl,
                                   login = login)
  
  
  rv |>
    qg::.composeVialSampleTable(orderID = orderId,
                                instrument = "ASTRAL",
                                user = login,
                                injVol = 1,
                                area = "Proteomics",
                                lc =  "Vanquish",
                                mode = instrumentMode,
                                randomization = FALSE) -> df
  
  expect_true(nrow(df) > 1)
  expect_true(ncol(df) > 8)
  
  
  do.call(what = qFUN, args = list(x = df,
                                         lc = "Vanquish",
                                         containerid = orderId,
                                         howOften = 1)) |>
    qg::.replaceRunIds() -> df2
  
  
  
  groundTruth <- c("1:F8", "1:F8", "Y:A1", "1:F8", "Y:A2",  "1:F8")
  expect_equal(df2$Position, groundTruth)
  
})

#R

testthat::context("Metabolomics")

testthat::test_that("test Metabolomics Vanquish VialXCaliburSII EquiSPLASH", {
  
  ####################################################
  orderId <- 37530
  instrumentMode <- ""
  qFUN <- "qconfigMetabolomicsVanquishVialXCaliburSIIEquiSPLASH"
  
  
  qg:::.readPackageFile('test-Metabolomics-Vanquish-VialXCaliburSII-EXPLORIS_4-c37530.RData') -> df

  howOften <- 16

  
  
  qg:::qconfigMetabolomicsVanquishVialXCaliburSIIEquiSPLASH(x = df,
                                                            mode = "pos",
                                                            containerid = orderId,
                                                            howOften = as.integer(16),
                                                            QCrow = "F") -> rv0
  do.call(what = qFUN,
          args = list(x = df,
                      containerid = orderId,
                      QCrow = "F",
                      mode = "pos",
                      howOften = as.integer(16))) -> rv1
  
  testthat::expect_true(all(rv0 == rv1, na.rm = TRUE))
  
})

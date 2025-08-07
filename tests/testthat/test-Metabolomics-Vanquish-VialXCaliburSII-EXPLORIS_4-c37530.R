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

testthat::test_that("test .alternatingPosNegSample function", {
  
  # Create test data frame
  test_df <- data.frame(
    `File Name` = c("sample1", "sample2"),
    `Sample Name` = c("test1", "test2"),
    `Position` = c("A:1", "A:2"),
    `Inj Vol` = c(10, 10),
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
  
  # Test the function
  result <- qg:::.alternatingPosNegSample(test_df)
  
  # Check dimensions
  testthat::expect_equal(nrow(result), 4)  # 2 * original rows
  testthat::expect_equal(ncol(result), ncol(test_df))
  
  # Check alternating pattern
  testthat::expect_equal(result[["File Name"]], c("sample1_pos", "sample1_neg", "sample2_pos", "sample2_neg"))
  testthat::expect_equal(result[["Sample Name"]], c("test1_pos", "test1_neg", "test2_pos", "test2_neg"))
  
  # Check other columns remain unchanged for corresponding rows
  testthat::expect_equal(result[["Position"]], c("A:1", "A:1", "A:2", "A:2"))
  testthat::expect_equal(result[["Inj Vol"]], c(10, 10, 10, 10))
  
})

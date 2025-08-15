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

testthat::test_that("test .lcVanquish function", {
  
  # Test with small number of vials
  result_10 <- qg:::.lcVanquish(10)
  
  # Check dimensions
  testthat::expect_equal(length(result_10), 10)
  
  # Check first few positions follow expected pattern
  testthat::expect_equal(result_10[1:5], c("Y:A1", "Y:A2", "Y:A3", "Y:A4", "Y:A5"))
  testthat::expect_equal(result_10[9:10], c("Y:A9", "Y:B1"))
  
  # Test with larger number (96 vials - full plate)
  result_96 <- qg:::.lcVanquish(96)
  testthat::expect_equal(length(result_96), 96)
  
  # Check that all positions are valid format
  testthat::expect_true(all(grepl("^[YRBG]:[ABCDE][1-9]$", result_96)))
  
  # Check plate transitions
  testthat::expect_equal(result_96[45], "Y:E9")  # Last position on first plate
  testthat::expect_equal(result_96[46], "R:A1")  # First position on second plate
  
  # Test default parameter
  result_default <- qg:::.lcVanquish()
  testthat::expect_equal(length(result_default), 10)
  testthat::expect_equal(result_default, result_10)
  
})

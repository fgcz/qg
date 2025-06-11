#R

context("container technology")

test_that("test container technology", {
  containerids <- c(35270, 35117, 31741)
  res <- bfabricShiny::read(login = login,
                            webservicepassword = webservicepassword,
                            posturl = posturl,
                            endpoint = "container",
                            maxitems = 100,
                            query = list('id' = containerids))$res

  groundtruths.technology <- c("Metabolomics/Biophysics", "Proteomics", "Metabolomics/Biophysics")
  
  output.containerid <-  sapply(res, function(x)x$id)
  output.technology <- sapply(res, function(x)x$technology[[1]])
  
  ## check order
  testthat::expect_equal(output.containerid, containerids)
  ## check technology
  testthat::expect_equal(groundtruths.technology , output.technology)
})

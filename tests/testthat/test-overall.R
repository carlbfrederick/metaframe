# Overall unit tests

context("Test that metaframes are data.frame compatible")

test_that("Basic functions do not throw errors", {
  data(airquality)
  outMD <- document(airquality)
  expect_is(outMD, "meta.data")
  expect_identical(slotNames(outMD), c("sources", "units", "description", "annotations", "revisions", 
                                       "var_names", "obs_names", "summary", "Rname",".S3Class"))
})
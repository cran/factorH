test_that("srh.kway requires numeric response", {
  data(mimicry, package = "factorH")
  expect_error(srh.kway(gender ~ condition, data = mimicry), "must be numeric")
})

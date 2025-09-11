test_that("srh.effsize returns augmented SRH table with call", {
  data(mimicry, package = "factorH")
  res <- srh.effsize(liking ~ gender + condition, data = mimicry)

  expect_s3_class(res, "data.frame")
  expect_true(all(c("k","n","eta2H","eps2H") %in% names(res)))
  expect_true(inherits(res, "srh_with_call"))
  expect_true(inherits(res, "anova"))

  # getCall works and contains formula pieces
  cl <- getCall(res)
  expect_true(inherits(cl, "call"))
  expect_true(grepl("liking ~ gender \\+ condition", deparse(cl)))

  # eta2H >= 0 when clamp0 = TRUE
  expect_true(all(res$eta2H[!is.na(res$eta2H)] >= 0))

  # eps2H in [0,1] when clamp0 = TRUE
  rng <- range(res$eps2H, na.rm = TRUE)
  expect_true(rng[1] >= 0 && rng[2] <= 1)

  # k is integer where defined
  expect_true(all(res$k[!is.na(res$k)] == as.integer(res$k[!is.na(res$k)])))
})

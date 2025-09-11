test_that("srh.simple.posthoc within vs global behave as expected", {
  data(mimicry, package = "factorH")

  w <- srh.simple.posthoc(liking ~ gender + condition, data = mimicry,
                          compare = "gender", scope = "within", digits = 3)
  g <- srh.simple.posthoc(liking ~ gender + condition, data = mimicry,
                          compare = "gender", scope = "global", digits = 3)

  expect_s3_class(w, "data.frame")
  expect_s3_class(g, "data.frame")

  # within: m.tests varies by subgroup (all equal here if balanced)
  expect_true(length(unique(w$m.tests)) >= 1)

  # global: m.tests equals total number of rows
  expect_true(all(g$m.tests == nrow(g)))
  expect_true(grepl("global", attr(g, "adjustment"), ignore.case = TRUE))
})

test_that("srh.simple.posthoc validates inputs", {
  data(mimicry, package = "factorH")
  expect_error(srh.simple.posthoc(gender ~ condition, data = mimicry),
               "must be numeric")
  expect_error(srh.simple.posthoc(liking ~ condition, data = mimicry),
               "at least two factors")
  # compare must be on RHS
  expect_error(srh.simple.posthoc(liking ~ gender + condition, data = mimicry,
                                  compare = "age_cat"),
               "must be one of")
})

test_that("srh.simple.posthoc produces expected columns", {
  data(mimicry, package = "factorH")
  out <- srh.simple.posthoc(liking ~ gender + condition, data = mimicry,
                            compare = "gender", scope = "within")
  need <- c("Comparison","Z","P.unadj","P.adj","m.tests","adj.note")
  expect_true(all(need %in% names(out)))
  expect_true(!is.null(attr(out, "adjustment")))
})

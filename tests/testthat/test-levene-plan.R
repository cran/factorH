test_that("levene.plan.datatable runs (Brown–Forsythe by default) on mimicry", {
  skip_if_not_installed("car")
  out <- levene.plan.datatable(liking ~ gender + condition + age_cat, data = mimicry)
  expect_true(all(c("Effect","n.groups","min.n","df.num","df.den","F","p","OK") %in% names(out)))
  expect_true(is.na(out$p) || grepl("^\\d+\\.\\d{1,4}$", out$p))
})

test_that("residuals.normality.datatable runs and formats p on mimicry", {
  out <- residuals.normality.datatable(liking ~ gender + condition + age_cat, data = mimicry)
  expect_true(all(c("Effect","count","W","p.shapiro","OK") %in% names(out)))
  expect_true(all(is.na(out$p.shapiro) | grepl("^\\d+\\.\\d{1,4}$", out$p.shapiro)))
})

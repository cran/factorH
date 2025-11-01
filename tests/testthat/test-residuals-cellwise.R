test_that("residuals.cellwise.normality.datatable yields per-cell rows on mimicry", {
  out <- residuals.cellwise.normality.datatable(liking ~ gender + condition + age_cat, data = mimicry)
  expect_true(nrow(out) >= length(table(mimicry$gender, mimicry$condition, mimicry$age_cat)))
  expect_true(all(c("Effect","count","W","p.shapiro","OK") %in% names(out)))
})

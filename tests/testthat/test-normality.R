test_that("normality.datatable returns expected structure on mimicry", {
  out <- normality.datatable(liking ~ gender + condition + age_cat, data = mimicry)
  expect_s3_class(out, "data.frame")
  expect_true(all(c("Effect","count","W","p.shapiro","OK") %in% names(out)))
  # p formatted to 4 decimals or NA
  ps <- out$p.shapiro
  expect_true(all(is.na(ps) | grepl("^\\d+\\.\\d{1,4}$", ps)))
  expect_true(all(out$OK %in% c("OK","NOT OK") | is.na(out$OK)))
})

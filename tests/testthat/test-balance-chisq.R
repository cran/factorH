test_that("balance.chisq.datatable works and formats on mimicry", {
  out1 <- balance.chisq.datatable(liking ~ gender, data = mimicry)
  out2 <- balance.chisq.datatable(liking ~ gender + condition, data = mimicry)
  out3 <- balance.chisq.datatable(liking ~ gender + condition + age_cat, data = mimicry)
  for (out in list(out1, out2, out3)) {
    expect_true(all(c("Effect","n","ChiSq","df","p.chisq","OK") %in% names(out)))
    expect_true(all(is.na(out$ChiSq)  | grepl("^\\d+\\.\\d{1,4}$", out$ChiSq)))
    expect_true(all(is.na(out$p.chisq) | grepl("^\\d+\\.\\d{1,4}$", out$p.chisq)))
  }
})

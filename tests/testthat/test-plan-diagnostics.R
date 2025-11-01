test_that("plan.diagnostics returns list with summary and results on mimicry", {
  # nie używamy expect_silent(), bo funkcja wypisuje summary na konsolę
  out_txt <- capture.output({
    diag_out <- plan.diagnostics(liking ~ gender + condition + age_cat, data = mimicry)
  })

  expect_type(diag_out, "list")
  expect_true(all(c("summary","results","call") %in% names(diag_out)))

  s <- diag_out$summary
  expect_true(all(c("percent_ok","ok_count","total","overall",
                    "percent_ok_normality_raw",
                    "percent_ok_residuals_cellwise",
                    "percent_ok_balance_chisq",
                    "percent_ok_levene_full_plan") %in% names(s)))

  r <- diag_out$results
  expect_true(all(c("normality_raw","residuals_cellwise_normality",
                    "levene_full_plan","balance_chisq") %in% names(r)))
})

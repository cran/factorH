test_that("write.srh.kway.full.tsv writes a TSV with headers and chosen decimal mark", {
  data(mimicry, package = "factorH")
  res <- srh.kway.full(liking ~ gender + condition, data = mimicry)

  # dot decimal
  f1 <- tempfile(fileext = ".tsv")
  p1 <- write.srh.kway.full.tsv(res, file = f1, dec = ".")
  expect_true(file.exists(p1))
  txt1 <- readLines(f1, warn = FALSE)
  expect_true(any(grepl("^## SRH: EFFECTS TABLE$", txt1)))
  expect_true(any(grepl("^## SUMMARY STATS", txt1)))
  expect_true(any(grepl("^## POSTHOC CELLS", txt1)))
  expect_true(any(grepl("^## SIMPLE EFFECTS", txt1)))
  expect_true(any(grepl("^## META$", txt1)))

  # comma decimal
  f2 <- tempfile(fileext = ".tsv")
  p2 <- write.srh.kway.full.tsv(res, file = f2, dec = ",")
  expect_true(file.exists(p2))
  txt2 <- readLines(f2, warn = FALSE)

  # heuristic: at least one numeric cell should contain a comma decimal
  # (skip header lines and empty lines)
  body2 <- txt2[!grepl("^##|^###|^# |^$", txt2)]
  expect_true(any(grepl("\\d,\\d", body2)))
})

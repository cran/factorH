test_that("srh.simple.posthocs returns all simple-effect combos with ASCII labels", {
  data(mimicry, package = "factorH")

  tabs <- srh.simple.posthocs(liking ~ gender + condition + age_cat, data = mimicry)
  expect_true(is.list(tabs))
  nm <- names(tabs)
  expect_true(all(grepl("^COMPARE\\(.+\\) \\| BY\\(.+\\)$", nm)))
  expect_false(any(grepl("\u00D7", nm)))  # no '×' character

  # spot-check a few expected names
  expect_true("COMPARE(gender) | BY(condition)" %in% nm)
  expect_true("COMPARE(gender) | BY(condition x age_cat)" %in% nm)
})

test_that("srh.simple.posthocs skips groups with <2 levels of target", {
  data(mimicry, package = "factorH")
  mm <- subset(mimicry, gender == "female")
  tabs <- srh.simple.posthocs(liking ~ gender + condition, data = mm)
  # 'COMPARE(gender) | BY(condition)' likely skipped; check attribute
  sk <- attr(tabs, "skipped")
  expect_true(is.null(tabs[["COMPARE(gender) | BY(condition)"]]))
  expect_true(any(grepl("COMPARE\\(gender\\) \\| BY\\(condition\\).*", sk)))
})

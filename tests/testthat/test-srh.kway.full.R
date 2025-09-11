test_that("srh.kway.full works for 1-, 2- and 3-factor designs", {
  data(mimicry, package = "factorH")

  # 1 factor
  f1 <- srh.kway.full(liking ~ condition, data = mimicry)
  expect_type(f1, "list")
  expect_true(all(c("anova","summary","posthoc_cells","posthoc_simple","meta") %in% names(f1)))
  expect_s3_class(f1$anova, "data.frame")
  expect_s3_class(f1$summary, "data.frame")
  expect_true(is.list(f1$posthoc_cells))     # matrices for effects
  expect_identical(f1$posthoc_simple, "[not applicable]")  # no simple effects with 1 factor
  expect_true(is.list(f1$meta))
  expect_equal(f1$meta$n, nrow(na.omit(mimicry[c("liking","condition")])))

  # 2 factors
  f2 <- srh.kway.full(liking ~ gender + condition, data = mimicry)
  expect_s3_class(f2$anova, "data.frame")    # from srh.effsize
  expect_true(is.list(f2$posthoc_cells))
  expect_true(all(c("gender","condition","gender:condition") %in% names(f2$posthoc_cells)))
  expect_true(is.list(f2$posthoc_simple))
  expect_true("COMPARE(gender) | BY(condition)" %in% names(f2$posthoc_simple))
  expect_true("COMPARE(condition) | BY(gender)" %in% names(f2$posthoc_simple))
  expect_true(is.list(f2$meta$empty_cells))  # present for >=2 factors

  # 3 factors
  f3 <- srh.kway.full(liking ~ gender + condition + age_cat, data = mimicry)
  expect_s3_class(f3$anova, "data.frame")    # from srh.kway
  expect_true(all(c("gender","condition","age_cat",
                    "gender:condition","gender:age_cat","condition:age_cat",
                    "gender:condition:age_cat") %in% names(f3$posthoc_cells)))
  expect_true(is.list(f3$posthoc_simple))
  expect_true("COMPARE(gender) | BY(condition x age_cat)" %in% names(f3$posthoc_simple))
})

test_that("srh.kway.full validates inputs and level counts", {
  data(mimicry, package = "factorH")

  # non-numeric response
  expect_error(srh.kway.full(gender ~ condition, data = mimicry),
               "must be numeric", ignore.case = TRUE)

  # factor with < 2 levels
  mm <- subset(mimicry, gender == "female")
  expect_error(srh.kway.full(liking ~ gender + condition, data = mm),
               "< 2 levels", ignore.case = TRUE)

  # factor with too many levels (force via small max_levels)
  expect_error(srh.kway.full(liking ~ condition, data = mimicry, max_levels = 1),
               ">* levels|> 1 levels", ignore.case = TRUE)
})

test_that("srh.kway.full returns data.frames for summary and anova", {
  data(mimicry, package = "factorH")
  f2 <- srh.kway.full(liking ~ gender + condition, data = mimicry)
  expect_s3_class(f2$summary, "data.frame")
  expect_true(all(c("Effect","count","mean","median","mean_rank") %in% names(f2$summary)))
})

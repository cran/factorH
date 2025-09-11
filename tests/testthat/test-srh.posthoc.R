test_that("srh.posthoc works for one factor and returns symmetric mats", {
  data(mimicry, package = "factorH")
  tabs <- srh.posthoc(liking ~ condition, data = mimicry, numeric = TRUE, triangular = "full")
  expect_true(is.list(tabs) && all(c("Z","P.unadj","P.adj") %in% names(tabs)))
  Z <- as.matrix(tabs$Z)
  P <- as.matrix(tabs$`P.adj`)
  # symmetry: Z_ij = -Z_ji ; P_ij = P_ji
  expect_true(all(abs(Z + t(Z)) < 1e-8, na.rm = TRUE))
  expect_true(all(abs(P - t(P)) < 1e-12, na.rm = TRUE))
  # diagonal NA when full mask not applied? We didn't touch diagonal here; Z_ii should be NA
  expect_true(all(is.na(diag(P))))
})

test_that("srh.posthoc masks triangles and formats blanks", {
  data(mimicry, package = "factorH")
  tabs <- srh.posthoc(liking ~ condition, data = mimicry, triangular = "lower", numeric = FALSE)
  # upper triangle should be blank strings
  m <- tabs$`P.adj`
  levs <- rownames(m)
  up_ids <- which(upper.tri(matrix(NA, length(levs), length(levs)), diag = TRUE), arr.ind = TRUE)
  expect_true(all(m[up_ids] == ""))
})

test_that("srh.posthoc validates numeric response and >=2 levels", {
  data(mimicry, package = "factorH")
  expect_error(srh.posthoc(gender ~ condition, data = mimicry), "must be numeric")
  expect_error(srh.posthoc(liking ~ field, data = subset(mimicry, field == field[1])), ">= 2 levels")
})

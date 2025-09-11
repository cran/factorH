test_that("nonpar.datatable returns expected columns and effects", {
  data(mimicry, package = "factorH")

  # One factor
  t1 <- nonpar.datatable(liking ~ condition, data = mimicry)
  expect_s3_class(t1, "data.frame")
  expect_true(all(c("Effect","condition","count","mean","sd","median","q1","q3","IQR","mean_rank") %in% names(t1)))
  expect_true(all(t1$Effect == "condition"))

  # Two factors: should contain rows for 'gender', 'condition', and 'gender:condition'
  t2 <- nonpar.datatable(liking ~ gender + condition, data = mimicry)
  expect_true(all(c("gender","condition") %in% names(t2)))
  expect_true(all(c("gender","condition","gender:condition") %in% unique(t2$Effect)))
})

test_that("nonpar.datatable uses global ranks", {
  data(mimicry, package = "factorH")
  # recompute global ranks manually
  d <- mimicry[complete.cases(mimicry[, c("liking","gender","condition")]), c("liking","gender","condition")]
  d$R <- base::rank(d$liking, ties.method = "average")

  # table from function
  t2 <- nonpar.datatable(liking ~ gender + condition, data = mimicry)
  # keep only rows for the interaction and compare a few cells
  inter <- subset(t2, Effect == "gender:condition")
  # pick one cell
  cell <- inter[1, c("gender","condition")]
  mr_fun <- inter$mean_rank[1]
  mr_ref <- mean(d$R[d$gender == cell$gender & d$condition == cell$condition])
  expect_equal(mr_fun, mr_ref)
})

test_that("nonpar.datatable validates numeric response", {
  data(mimicry, package = "factorH")
  expect_error(nonpar.datatable(gender ~ condition, data = mimicry), "must be numeric")
})

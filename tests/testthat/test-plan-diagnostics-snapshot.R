test_that("plan.diagnostics prints concise summary on mimicry", {
  expect_snapshot_output(plan.diagnostics(liking ~ gender + condition, data = mimicry))
})

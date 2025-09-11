test_that("srh.posthocs returns p.adj tables for all effects", {
  data(mimicry, package = "factorH")

  L2 <- srh.posthocs(liking ~ gender + condition, data = mimicry, numeric = TRUE, triangular = "full")
  expect_true(is.list(L2))
  expect_true(all(c("gender","condition","gender:condition") %in% names(L2)))
  # macierze kwadratowe i symetryczne po masce (full => bez maski)
  for (nm in names(L2)) {
    M <- as.matrix(L2[[nm]])
    expect_equal(nrow(M), ncol(M))
    expect_true(all(abs(M - t(M)) < 1e-12, na.rm = TRUE))
    expect_true(all(is.na(diag(M))))  # diagonal powinna być NA
  }

  L3 <- srh.posthocs(liking ~ gender + condition + age_cat, data = mimicry)
  expect_true(all(c("gender","condition","age_cat",
                    "gender:condition","gender:age_cat","condition:age_cat",
                    "gender:condition:age_cat") %in% names(L3)))
})

test_that("srh.posthocs handles effects with <2 levels by skipping", {
  data(mimicry, package = "factorH")
  # sztucznie zredukuj jedno ze zjawisk do 1 poziomu
  mm <- subset(mimicry, gender == "female")
  L <- srh.posthocs(liking ~ gender + condition, data = mm)
  # 'gender' powinno zostać pominięte (brak 2 poziomów)
  expect_false("gender" %in% names(L))
  # inne efekty powinny pozostać
  expect_true(all(c("condition") %in% names(L)))
})

test_that("SMMAL returns correct structure", {
  file_path <- system.file("extdata", "sample_data.rds", package = "SMMAL")
  dat <- readRDS(file_path)

  out <- SMMAL(N = 1000, Y = dat$Y, A = dat$A, S = data.frame(dat$S), X = data.frame(dat$X))
  expect_true(is.list(out))
  expect_named(out, c("est", "se"))
})

test_that("NFoldManifold constructor works", {
  nfm <- NFoldManifold$new(
    base_manifold = SPDMatrices$new(n = 3),
    n_copies = 3
  )
  expect_true("NFoldManifold" %in% class(nfm))
})

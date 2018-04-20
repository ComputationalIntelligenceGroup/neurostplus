context("XYZ")

test_that("PCA direction", {
  cell <- 'C010600A2'
  f <- get_test_swc(cell)
  nodes <- compute_primitives_node(f)
  nodes <- filter_neurite(nodes)
  pca <- quantify_xyz(nodes)
  # 0.2464139 is the value computed prior to refactoring
  expect_equal(pca[['eccentricity']], 0.2464139, tolerance = 1e-6)
  expect_true(pca[['radial']] >= -1 && pca[['radial']] <= 1)
})


test_that('mean direction', {
  f <- get_test_swc('C010600A2')
  nodes <- compute_primitives_node(f)
  nm <- norm_by_row(nodes[ , c('x', 'y', 'z')])
  nms <- apply(as.matrix(nm), 1, norm, type = '2')
  expect_equal(unname(nms), rep(1, length(nms)))
})

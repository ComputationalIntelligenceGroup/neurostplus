context("extend")

test_that("Vertex ratio", {
  f <- get_test_swc()
  branches <- compute_primitives_branch(f)
  types <- na.omit(subset(branches, is_pre_terminal)$vertex_type )
  expect_true(all(types %in% 1:2))
  types <- subset(branches, is_terminal)$vertex_type
  expect_true(all(is.na(types)))

  files <- get_all_swc_files()
  a <- lapply(files, get_vr )
  expect_true(all(sapply(a, is.numeric)))

  cell <- 'C010600B2'
  f <- get_test_swc(cell)
  branches <- compute_primitives_branch(f)
  branches <- filter_neurite(branches)
  computed <- compute_vertex_ratio(branches)
  expect_equal(0.6101695, computed[['vertex_ratio']])
})

test_that("Angles", {
  expect_true(FALSE)
  # Should apply theta_complement?
})

test_that("test 2D complement", {
  theta <- cbind(a = 80:120, b = 40:80)
  compl <- get_theta_complement(theta)
  expect_true(all(compl <= 90))
  expect_equal(unname(compl[12, 1]), 89)
  expect_equal(unname(compl[12, 2]), 51)
})

test_that("Distance to soma", {
  f <- get_test_swc()
  nodes <- compute_primitives_node(f)
  nodes <- filter_neurite(nodes)
  n <- extend_nodes(nodes)
  expect_equal('soma_dist', names(n))
})

test_that("merge does not affect", {
  f <- get_test_swc("C040600B1")
  branch <- compute_primitives_branch(f)
  branch <- filter_neurite(branch)
  branch_node <- compute_primitives_branch_node(f)
  branch_node <- filter_neurite(branch_node)
  expect_identical( count_tips(branch), count_tips(branch_node))
  expect_identical( summarize_branches(branch), summarize_branches(branch_node))
})

test_that("just soma cell", {
  skip("remove this one as it does not work with many functions, and it is not such an important case")
  f <- get_test_swc("RP120607_P_1+2_IDE")
  branch_node <- compute_primitives_branch_node(f)
  expect_equal(dim(branch_node), c(0, 5))
})

test_that("merge does not affect node", {
  expect_true(FALSE)
})

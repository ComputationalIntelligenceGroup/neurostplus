context("quantify")

test_that("quantify branch node", {
  f <- get_test_swc()
  branch_node <- compute_primitives_branch_node(f)
  branch_node <- filter_neurite(branch_node)
  quantify_branch_node(branch_node)

})

test_that("quantify xyz", {
  f <- get_test_swc()
  nodes <- compute_primitives_node(f)
  nodes <- filter_neurite(nodes, axon = FALSE)
  avg_dir <- quantify_xyz(nodes)
  expect_equal(length(avg_dir), 55)
})


test_that("XYZ", {
  f <- get_test_swc()
  node <- compute_primitives_node(f)
  node <- filter_neurite(node, axon = FALSE)
  xyz <- quantify_xyz(node)
  expect_is(xyz, 'numeric')
})

test_that("laminar", {
  f <- get_test_swc()
  branch_node <- compute_primitives_branch_node(f)
  axon <- neurostr::filter_neurite(branch_node)
  custom <- quantify_laminar(axon, layer = '23', get_thickness_mean(), get_thickness_sd())
  expect_equal(length(custom), 8)
  names <- names(custom)
  expect_true(length(names) > 0  && !anyNA(names))
  expect_true(all(c('l1_prob', 'translaminar') %in% names))
})

test_that("N stems", {
 # f <- get_test_swc(list_test_swc_ids()[5])
 branch <- compute_primitives_branch(f)
 dendrite <- neurostr::filter_neurite(branch, axon = FALSE)
 dendrite <- quantify_branches(dendrite )
 expect_is(dendrite, 'numeric')
 expect_equal(dendrite[['N_stems']], 9.0)
})


test_that("All branch node, including derived", {
  # this will include custom except for laminar and dendritic which go apart.
  f <- get_test_swc()
  branch_node <- compute_primitives_branch_node(f)
  axon <- filter_neurite(branch_node )
  measures <- quantify_branch_node(axon)
  expect_true(is.numeric(measures))
  # todo: check the documented neurostr metrics are included
  # not doing it because will move that code to gabaclassifier
})

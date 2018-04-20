context("gaba and domains")

test_that("quantify gaba nominal", {
  f <- get_test_swc()
  measures <- quantify_gaba(f, '23', get_thickness_mean(), get_thickness_sd())
  expect_true(is.numeric(measures))
  expect_equal(length(measures), 86)
  expect_true(all(selected_neurostr_vars() %in% names(measures)))
# - todo: check for NA data in the computed values
})

test_that("quantify gaba layer", {
  f <- get_test_swc()
  expect_error(quantify_gaba(f, '1', get_thickness_mean(), get_thickness_sd()))
})

test_that("quantify gaba layer 2", {
  f <- get_test_swc()
  measures <- quantify_gaba(f, '2', get_thickness_mean(), get_thickness_sd())
  expect_true(is.numeric(measures))
})

test_that("quantify axon nominal", {
  f <- get_test_swc()
  branch_node <- compute_primitives_branch_node(f)
  measures <- quantify_gaba_axon(branch_node, '6', get_thickness_mean(), get_thickness_sd())
  expect_true(is.numeric(measures))
  d <- selected_custom_vars()
  inmeasures <-  d %in% names(measures)
  notin <-d[!inmeasures]
  # check which selected vars are included. only dendritic should miss from here.
  expect_equal(notin, c("insert.eccentricity", "insert.radial", "displaced"))
})

test_that("quantify dendrite nominal", {
  f <- get_test_swc()
  branch_node <- compute_primitives_branch_node(f)
  measures <- quantify_gaba_dendrites(branch_node)
  expect_true(is.numeric(measures))
  expect_true(all(c("insert.eccentricity", "insert.radial") %in% names(measures)))
  # displaced is currently missing
  expect_true(all(c( "displaced") %in% names(measures)))
})

test_that("y_std_mean_abs", {
  "Why is this one missing?"
  expect_true(FALSE)
})

test_that("check recon", {
  checks <- lapply(get_all_swc_files(),  check_reconstruction, '23')
  expect_true(FALSE)
})

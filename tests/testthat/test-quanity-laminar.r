context("laminar")

test_that("Wrong layer", {
   f <- get_test_swc()
   nodes <- compute_primitives_node(f)
   nodes <- filter_neurite(nodes)
   expect_error(p <-  prob_l1(neuron = nodes, '1', get_thickness_mean(), get_thickness_sd()), "Layer must be one of: 23, 4, 5, 6.")
})

test_that("Laminar reach", {
   f <- get_test_swc()
   nodes <- compute_primitives_node(f)
   nodes <- filter_neurite(nodes)
   check_probs_layer(nodes)

   f <- get_test_swc("C020600C1")
   nodes <- compute_primitives_node(f)
   nodes <- neurostr::filter_neurite(nodes)
   check_probs_layer(nodes)

   f <- get_test_swc("C031097B-I4")
   nodes <- compute_primitives_node(f)
   nodes <- filter_neurite(nodes)
   nodes <- neurostr::filter_neurite(nodes)
   check_probs_layer(nodes)

   f <- get_test_swc("C010600B2")
   nodes <- compute_primitives_node(f)
   nodes <- filter_neurite(nodes)
   nodes <- neurostr::filter_neurite(nodes)
   check_probs_layer(nodes)


   f <- get_test_swc("C040426")
   nodes <- compute_primitives_node(f)
   nodes <- filter_neurite(nodes)
   check_probs_layer(nodes)

})

test_that("Laminar reach", {
  expect_true(FALSE)
})

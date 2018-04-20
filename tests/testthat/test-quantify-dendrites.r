context("dendrites")

test_that("insertion points nominal", {
 f <- get_test_swc()
 node <- compute_primitives_node(f)
 insertion <- get_insertion_points(node)
 expect_true(is.matrix(insertion))
 expect_equal(nrow(insertion), 9)
})

test_that("compute polarity", {
 f <- get_test_swc()
 node <- compute_primitives_node(f)
 polarity <- compute_dendritic_polarity(node)
 expect_equal(length(polarity), 2)
 expect_true(FALSE)
})

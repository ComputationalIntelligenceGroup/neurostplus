library(neurostr)
library(neurostrplus)

get_test_swc <- function(id = NULL) {
  if (is.null(id)) id <- "C040600B1"
  id <- paste0(id, ".swc")
  f <- system.file("extdata/swc", id, package = "neurostrplus")
  stopifnot(file.exists(f))
  f
}

list_test_swc_ids <- function() {
  f <- system.file("extdata/swc", package = "neurostrplus")
  f <- list.files(f)
  gsub('\\.swc$', '', f)
}


get_all_swc_files <- function() {
  files <- list_test_swc_ids()
  sapply(files, get_test_swc)
}

get_thickness_mean <- function() {
  t <- c(165, 149 + 353, 190, 525, 700)
  names(t) <- c('1', '23', '4', '5', '6')
  t
}

get_thickness_sd <- function() {
   s <- c(13, 27, 7, 33, 48)
   names(s) <- c('1', '23', '4', '5', '6')
   s
}

check_probs_layer <- function(nodes) {
  p <- prob_l1(neuron = nodes, '23', get_thickness_mean(), get_thickness_sd())
  r <- prob_l1(neuron = nodes, '4', get_thickness_mean(), get_thickness_sd())
  q <- prob_l1(neuron = nodes, '6', get_thickness_mean(), get_thickness_sd())
  expect_true(p > r || all.equal(p, r, tolerance = 1e-5))
  expect_true(r > q || all.equal(r, q, tolerance = 1e-5))
}

get_vr <- function(f) {
    branches <- compute_primitives_branch(f)
    branches <- filter_neurite(branches)
    compute_vertex_ratio(branches)
}

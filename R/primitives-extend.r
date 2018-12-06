# these functions compute values, do not add them to data frames .

#' summarize
#' seems i require is terminal etc. that is for the branch to have been extended.
#' add that to the neurostr tools? or not?
#' seems it is already added.
#' @export
extend_branches <- function(branch) {
  c(summarize_branches(branch),  compute_vertex_ratio(branch))
}
#' @export
extend_nodes <- function(node) {
  c( distance_to_soma(node))
}
#' Summarize branches
#' @return named numeric vector
summarize_branches <- function(branch) {
  stopifnot(is_unique_neuron(branch) && is_unique_neurite_type(branch))
  N_tips <- count_tips(branch)
  N_bifurcations <- count_bifs(branch)
  N_stems <- count_stems(branch)
  vars <- c(N_stems, N_tips, N_bifurcations)
  names(vars) <- c('N_stems' , 'N_tips' , 'N_bifurcations')
  vars
}
#' Computes vertex ratio
#' Neurolucida docs: \url{https://www.mbfbioscience.com/help/nx11/Content/Analyses/Vertex_Analysis.htm}
#' Va: primary vertices connecting 2 pendant vertices (Vp).
#' Vb: secondary vertices connecting 1 pendant vertex (Vp) to 1 bifurcation (Vd) or 1 trifurcation (Vt).
#'
#' Va/Vb>1 suggests that the tree is non-random and symmetrical.
#' Va/Vb ~ 1 suggests that terminal nodes grew in random processes.
#' Va/Vb~ 0.5 suggest that segment growth grew the tree.
#' Va/Vb< 0.5 suggests the tree is non-random and asymmetrical
#' @return named numeric vector
compute_vertex_ratio <- function(branch) {
  stopifnot(is_unique_neuron(branch) && is_unique_neurite_type(branch))
  primary <- sum(branch$vertex_type == 2, na.rm = TRUE)
  secondary <- sum(branch$vertex_type == 1, na.rm = TRUE)
  vertex_ratio <- primary / secondary
  c(vertex_ratio = vertex_ratio, vertex_type_two = type_two, vertex_type_one = type_one )
}
distance_to_soma <- function(node) {
  stopifnot(is_unique_neuron(node) && is_unique_neurite_type(node))
  soma_dist <- node[1, 'node_length']
  vars <- c( soma_dist)
  names(vars) <- c('soma_dist')
  vars
}

#' Summarize for a neuron
#' @return named numeric vector
#' renamed from summarize both
summarize_measures <- function(both) {
  measures <- neurostr::filter_id(both, keep = FALSE)
  measures <- filter_dont_summarize(measures )
  vars <- lapply(measures, summarize_dist)
  vars <- unlist(vars)
  names(vars) <- gsub('^length.sum$', 'total_length', names(vars))
  # vars <- gsub('^node_length.sum$', 'total_length', names(vars))
  vars
}
summarize_dist <- function(x) {
 suppressWarnings(c( min = min(x, na.rm = TRUE), med = median(x, na.rm = TRUE), avg = mean(x, na.rm = TRUE), max = max(x, na.rm = TRUE), sum = sum(x, na.rm = TRUE),
     sd = sd(x, na.rm = TRUE), kurtosis = moments::kurtosis(x, na.rm = TRUE), skewness = moments::skewness(x, na.rm = TRUE)))
}

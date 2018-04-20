#'  Also keep neurite type? Make it optional argument?
#' TODO: move to neurostr!!
#' @export
filter_xyz <- function(both) {
  if (identical(c('x', 'y', 'z'), colnames(both))) {
     return(both)
  }
  stopifnot(is_no_or_unique_neuron(both))
  both[, c('x', 'y', 'z')]
}

#' Measures not to summarize
#' Contains both branch and node measures
#' TODO: move to neurostr!!
#' @export
filter_dont_summarize <- function(branch_node) {
  remove <- c('is_terminal', 'is_pre_terminal', 'is_initial', 'x', 'y', 'z', 'n_tips_left', 'n_tips_right', 'vertex_type', 'change_x', 'change_y', 'change_z')
  remove <- c(remove, c("extreme_angle", "soma_dist", "N_descs", "box_volume", "local_bifurcation_angle", "node_local_elongation", "node_local_orientation"))
  keep <-  setdiff(colnames(branch_node), remove )
  branch_node[ , keep, drop = FALSE ]
}

# Count neurons
#' @export
count_neurons <- function(data) {
  length(unique(data$neuron))
}

#' Count stems
#' @export
count_stems <- function(data) {
  length(unique(data$neurite))
}

#' Count tips
#' @export
count_tips <- function(data) {
  sum(data$is_terminal, na.rm = TRUE)
}

#' Count neurite types
#' @export
count_neurite_types <- function(data) {
  length(unique(data$neurite_type))
}

#' @export
is_unique_neuron <- function(primitives) {
  count_neurons(primitives)  == 1
}

#' Check a single neuron or no rows
#' at least move the is_unique_neuron(xyz) function with length(unique(xyz$neuron)) == 1 )
#' @export
is_no_or_unique_neuron <- function(primitives) {
  (nrow(primitives) == 0  || is_unique_neuron(primitives))
}


#' @export
is_unique_neurite_type <- function(primitives) {
  types <- sort(unique(primitives$neurite_type))
  n <-  length(types)
  (n == 1) || (identical(types, c("Apical", "Dendrite")))
}

#' Count bifurcations
#' @export
count_bifs <- function(data) {
  length(unique(data$branch))
}

#' TODO:  Fix this in neurostr, i.e., make them logical
#' #' flags to logical
flag_branches <- function(branch) {
  if (length(branch) > 0) {
    id_len <- sapply(branch$branch, nchar)
    branch$is_initial <- id_len == 1
    branch$is_terminal <- as.logical(branch$is_terminal)
    branch$is_pre_terminal <- as.logical(branch$is_pre_terminal)
  }
  branch
}

#' Get complementary angles for a matrix of angles
#' Angles muy be in degrees
#' @export
get_theta_complement <- function(angles, sum = 180) {
    vect <- unlist(angles)
    angles[] <- pmin(vect, 180 - vect)
    angles
}

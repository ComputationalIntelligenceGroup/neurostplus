#' Quantify all for a given domain. This does not include dendritic functions, as they are a specific domain!!
#' Common for all domains
#' todo: also mean tree length could be somewhere here. fix total length?
#' @export
quantify_branch_node <- function(branch_node) {
  branch_vars  <- extend_branches(branch_node)
  measures <- summarize_measures(branch_node)
  node_vars  <- extend_nodes(branch_node)
  xyz <- quantify_xyz(branch_node)
  custom <- quantify_petilla_neurtral(branch_node)
  # todo: missing custom features. yet some are laminar some are domain specific. such as dendritic and axon origin.
  # todo:axon origin: check axon, as dendritic checks it is dendrites.
  metrics <- c(branch_vars, measures, node_vars, xyz, custom)
  derived <- compute_derived(metrics)
  c(metrics, derived )
}

#' Quantify the morphometrics of a set of branches
#' @export
quantify_branches <- function(branch) {
  # make a vector of summaries
  additional <- extend_branches(branch)
  # add additional to a vector
  measures <- summarize_measures(branch)
  c(measures, additional)
}

#' Quantify the morphometrics of a set of nodes
#' @export
quantify_nodes <- function(node) {
  # make a vector of summaries
  additional <- extend_nodes(node)
  # add additional to a vector
  measures <- summarize_measures(node)
  c(measures, additional)
}

#' Quantify xyz with node
#' Require node, although it really needs just neurite type and XYZ
#' @export
quantify_xyz <- function(node) {
  stopifnot(is_unique_neuron(node), is_unique_neurite_type(node))
  #  this does not even require neurostr. although it can use it as input. just need to parse swc. however, neurostr can let us parse even other formats.
  # howeve,r
  # no need to assume that his is a
  xyz <- filter_xyz(node)
  avg <- compute_com(xyz)
  pca <- compute_pca_vars(xyz)
  bound <- compute_bounds_moments(xyz)
  overall_direction <- compute_mean_direction(xyz)
  c(com = avg, pca, bound, overall = overall_direction)
}

#' Quantify custom vars
#' @export
quantify_laminar <- function(branch_node, layer, thickness_mean, thickness_sd, mc_axonal_pattern = TRUE) {
  l1 <- prob_l1(branch_node, layer, thickness_mean, thickness_sd)
  trans <- compute_prob_translaminar(branch_node, layer, thickness_mean, thickness_sd, assume_middle = TRUE)
  vars <- c(l1, trans)
  if (mc_axonal_pattern ) {
    mc_l1 <- compute_mc_l1(branch_node, layer, thickness_mean, thickness_sd)
    vars <- c(vars, mc_l1)
  }
  vars
}
#' Quantify axon-specific vars
quantify_axonal <- function(neuron) {
  compute_origin(neuron)
}
quantify_dendritic <- function(node) {
  # bipolarity
  polarity <- compute_dendritic_polarity(node)
  # TODO: displaced
#   load('mdist.rdata')
#   mdist <- mdist[rownames(db)]
  c(polarity)
}

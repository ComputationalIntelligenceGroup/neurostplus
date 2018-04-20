# domains. axon, dendrites, pre-terminal
# select variables and format

# TODO: move this file to gabaclassifier

#' A specific quantification of metrics for gabaergic interneurons
#' @export
quantify_gaba <- function(file, layer, thickness_mean, thickness_sd) {
  layer <- format_layer(layer)
  branch_node <- compute_primitives_branch_node(file)
  axon <- quantify_gaba_axon(branch_node, layer, thickness_mean, thickness_sd)
  dendrites <- quantify_gaba_dendrites(branch_node)
  terminal <- quantify_gaba_axon_terminal(branch_node)
  # filter unused metrics and moments
  data <- c(axon, t = terminal, d = dendrites)
  vars_custom <- selected_custom_vars()
  vars_neurostr <- selected_neurostr_vars()
  data <- data[c(vars_neurostr, vars_custom)]
  data
}

quantify_gaba_axon <- function(branch_node, layer, thickness_mean, thickness_sd) {
  axon <- filter_neurite(branch_node )
  rm(branch_node)
  measures <- quantify_branch_node(axon)
  laminar <- quantify_laminar(axon, layer, thickness_mean, thickness_sd)
  specific  <- quantify_axonal(axon)
  c(measures, laminar, specific)
}

quantify_gaba_axon_terminal <- function(branch_node) {
  axon <- filter_neurite(branch_node )
  rm(branch_node)
  # filter terminal function
  subset <- subset(axon, is_terminal)
  # todo: filter preterminal function
  terminal <- quantify_branch_node(subset )
  subset <- subset(axon, is_pre_terminal)
  pre_terminal <- quantify_branch_node(subset)
  stopifnot(!isTRUE(all.equal(terminal, pre_terminal)))
  terminal <- terminal[c('length.med', 'length.avg', 'tortuosity.avg')]
  pre_terminal <- pre_terminal[grep('angle', names(pre_terminal))]
  c(terminal, pre_terminal)
}

quantify_gaba_dendrites <- function(branch_node) {
  dendrite <- filter_neurite(branch_node, axon = FALSE)
  rm(branch_node)
  general <- quantify_branch_node(dendrite)
  specific <- quantify_dendritic(dendrite)
  c(general, specific)
}

#   variables selected in the bbp paper
#   have changed the names in the paper
#   vars <- gsub('^node_length$', 'compartment_length', vars)
#   vars <- gsub('^node_root_dist$', 'euclidean_dist', vars)
#   vars <- gsub('^node_root_path$', 'path_dist', vars)
selected_neurostr_vars <- function() {
 vars <- c("centrifugal_order.avg", "centrifugal_order.max", "centrifugal_order.sd", "node_root_dist.avg", "node_root_dist.max", "node_root_dist.sd", "height", "length.avg", "length.med", "length.sd",
           "N_bifurcations", "N_stems", "partition_asymmetry.avg", "node_root_path.avg", "node_root_path.max", "node_root_path.sd", "remote_bifurcation_angle.avg", "remote_tilt_angle.avg",
           "remote_torque_angle.avg", "terminal_degree.avg", "tortuosity.avg", "tortuosity.med", "total_length", "tree_length.avg", "vertex_ratio", "width")
 axon <- setdiff(vars, 'N_stems')
 terminal <- c("t.length.med", "t.length.avg", "t.tortuosity.avg", "t.remote_bifurcation_angle.avg", "t.remote_tilt_angle.avg", "t.remote_torque_angle.avg")
 dendrites <-setdiff(vars, 'vertex_ratio')
 dendrites  <- paste('d.', dendrites, sep = "")
 c(axon, terminal, dendrites  )
}

#   variables selected in the bbp paper
selected_custom_vars <- function() {
  c("density_area", "density_bifs", "density_dist", "short_vertical_terminals", "displaced", "insert.eccentricity", "insert.radial", "l1_prob", "translaminar", "l1_bifs", "l1_gx", "l1_gxa", "l1_width", "axon_above_below", "axon_origin", "grid_area", "grid_density", "grid_mean", "ratio_x", "ratio_y", "x_mean", "x_mean_abs", "x_sd", "y_mean", "y_mean_abs", "y_sd", "y_std_mean", "y_std_mean_abs", "eccentricity", "radial")
}

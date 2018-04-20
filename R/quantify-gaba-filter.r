# list of id and the failing check..
# TODO: documentation in gabaclassifier documentation. And reference it from here.
#' @export
check_reconstruction <- function(file, layer) {
  valid <- check_neurostr(file, layer)
  valid <- subset(valid, name %in% c('Neurites are attached to soma', 'Neuron has soma'))
  # These might be useful but I am not sure how are they defined: 'Planar neurite validation', 'Extreme angles validator'
  custom <- check_custom(file, layer)
  rbind(valid, custom)
}
check_neurostr <- function(file, layer) {
  valid <- neurostr::json2dataframe(neurostr::validate(file))
  valid <- valid[ , c('name' , 'pass')]
  valid
}
check_custom <- function(file, layer) {
  branch_node <- compute_primitives_branch_node(file)
  output <- logical()
  output['Layer 2/3-6'] <- layer %in% list_real_layers()
  # have both axon and dendrites
  axon <- filter_neurite(branch_node)
  dendrites <- filter_neurite(branch_node, axon = FALSE)
  has_axon <- nrow(axon) > 0
  output['Has axon and dendrites'] <- has_axon && nrow(dendrites) > 0
  output[c('Single axon', 'Axon > 3000 microns')] <- FALSE
  if (has_axon) {
    # this would fail without an axon
    metrics <- quantify_branch_node(axon)
    ## unique axon
    output['Single axon'] <- metrics['N_stems'] == 1
    output['Axon > 3000 microns'] <- metrics['total_length'] > 3000
  }
  # TODO is apical OK?
  # depth z? segment length, anomalies?
  output <- data.frame(name = names(output), pass = unname(output))
  output
}

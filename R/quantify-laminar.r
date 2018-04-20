list_all_layers <- function() {
  c('1', '23', '4','5', '6')
}
list_real_layers <- function() {
  list_all_layers()[-1]
}
#' Correct layer format
#' @export
format_layer <- function(layer) {
  layer <- trimws(layer)
  if (layer %in% c('2', '3')) return('23')
  layer
}
valid_layer_data <- function(layer, thicknesses, sds) {
 # A possible user error; the below thicknesses data are usually not provided by user.
 if (!layer %in% list_real_layers()) {
   stop(paste0("Layer must be one of: ", paste0(list_real_layers(), collapse = ", "), "."))
 }
 length(thicknesses ) == length(sds) && identical(names(thicknesses), list_all_layers()) && identical(names(thicknesses), names(sds)) &&
    is.character(layer)
}
#' Prob of L1
#' Assume soma centroid is at 0, 0, 0
prob_l1 <- function(neuron, layer, thicknesses, sds) {
  stopifnot(is_unique_neuron(neuron), is_unique_neurite_type(neuron), valid_layer_data(layer, thicknesses, sds) )
  dist_to_l1 <- layer_to_l1(layer, thicknesses)
  maxy <- max(neuron[, 'y'])
  sds_l1 <- layer_to_l1_sd(layer, sds)
  psd <- soma_to_center_sd(layer, thicknesses)
  var <- sqrt(sds_l1 ^ 2 + psd ^ 2)
  prob <- pnorm(maxy, mean = dist_to_l1, sd = var)
  c(l1_prob = prob)
}
soma_to_center_sd <- function(cell_layer, thicknesses) {
  stopifnot(identical(names(thicknesses), list_all_layers()))
  ind <- match(cell_layer, list_all_layers())
  thicknesses[ind] / 4
}
#' H to pia
#' Sum of half L1 + above layers and optionally half its own layer
layer_to_l1 <- function(cell_layer, thicknesses) {
  stopifnot(identical(names(thicknesses), list_all_layers()))
  ind <- match(cell_layer, list_real_layers())
  inds <- seq_len(ind)[-1]
  thicknesses[1] / 2 + sum(thicknesses[inds]) + thicknesses[ind + 1] / 2
}
layer_to_l1_sd <- function(cell_layer, sds) {
  stopifnot(identical(names(sds), list_all_layers()))
  ind <- match(cell_layer, list_all_layers())
  stopifnot(ind > 1)
  var <- sds ^ 2
  inds <- seq_len(ind)[-c(1, ind)]
  var <- (var[1]  / 4)  +  sum(var[inds])  + ( var[ind]  / 4)
  sqrt(var)
}
#' Compute translaminar
compute_prob_translaminar <- function(neuron, layer, thicknesses, sds, assume_middle = assume_middle) {
  stopifnot(is_unique_neuron(neuron), is_unique_neurite_type(neuron), valid_layer_data(layer, thicknesses, sds) )
  thickness <-  thicknesses[layer]
  sd <- sds[layer]
  above_height <- max(max(neuron[, 'y']), 0)
  pabove <- compute_prob_translaminar_side(above_height, thickness, sd)
  below_height <- abs(min(min(neuron[, 'y']), 0))
  pbelow <- compute_prob_translaminar_side(below_height, thickness, sd)
  if (layer == '6') pbelow <- 0
  prob <- max(pabove, pbelow)
  c(translaminar = prob)
}
compute_prob_translaminar_side <- function(height, thickness, sd) {
  pnorm(height, mean = thickness / 2, sd = sd / 4)
}

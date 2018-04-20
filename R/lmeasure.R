map_neurostr_lmeasure <- function() {
  lm <- c('Partition_asymmetry',     'PathDistance',   'Fractal_Dim',              'Branch_pathlength', 'Contraction',      'EucDistance',     'Length',   'Branch_Order' )
  ns <- c('partition_asymmetry',   'node_root_path', 'fractal_dimension', 'length',     'tortuosity', 'node_root_dist', 'node_length', 'centrifugal_order')
  lm <- c(lm, 'Bif_torque_remote',   'Bif_ampl_remote',          'Bif_tilt_remote')
  ns <- c(ns, 'remote_torque_angle', "remote_bifurcation_angle", "remote_tilt_angle")
  lm <- c(lm, 'Bif_torque_local',   'Bif_ampl_local',          'Bif_tilt_local')
  ns <- c(ns, 'local_torque_angle', "local_bifurcation_angle", "local_tilt_angle")

  lm_moments  <- c('min', 'avg', 'std', 'max', 'sum')
  lm <- lapply(lm, paste0, '_', lm_moments)
  lm <- unlist(lm, use.names = FALSE)
  ns_moments <-  c('min', 'avg',  'sd', 'max', 'sum')
  ns <- lapply(ns, paste0, '.', ns_moments)
  ns <- unlist(ns, use.names = FALSE)
  lm <- c(lm, 'Fragmentation_sum', 'N_bifs_sum')
  ns <- c(ns, 'N_nodes', 'N_bifurcations')
  # custom computed
  lm <- c(lm, 'Height_sum', 'Width_sum', 'Depth_sum')
  ns <- c(ns, 'height', 'width', 'depth')
  setNames(lm, ns)
}
#' Works both for axon and dendrites
#' @export
convert2lm <- function(branch) {
  tortuosity <- grep('tortuosity$', colnames(branch))
  if  (length(tortuosity) > 0) {
    branch[ , tortuosity] <- 1 / branch[ , tortuosity]
  }
  angles <- grep('_angle$', colnames(branch))
  if  (length(angles) > 0) {
    branch[ , angles] <- branch[ , angles] / pi * 180
  }
  # Torque: make all angles <= 90
  torques <- grep('_torque_angle$', colnames(branch))
  if  (length(torques) > 0) {
    branch[ , torques] <- get_theta_complement(branch[ , torques])
  }
  # TODO: distance = distance - 1st component?
  branch
}
#' Return a db with corresponding columns
#'
#' Kepp LM names.
#'
#' @export
intersect_lm <- function(lm, ns) {
  stopifnot(identical(rownames(lm), rownames(ns)))
  map <- map_neurostr_lmeasure()
  map <- map[map %in% colnames(lm)]
  map <- setNames(names(map), unname(map))
  map[map %in% colnames(ns)]
}
#' Compute LM difference.
#' @export
compute_lm_difference <- function(lm, ns, cor = FALSE) {
  cols <- intersect_lm(lm, ns)
  lm <- lm[, names(cols), drop = FALSE]
  ns <- ns[, cols, drop = FALSE]
  if (cor) {
   sapply(seq_len(ncol(lm)), function(ind) cor(lm[, ind], ns[, ind]))
  }
  else {
    lm - ns
  }
}

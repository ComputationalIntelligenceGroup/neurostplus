ratio <- function(instance, vara, varb) {
 stopifnot(vara %in% names(instance), varb %in% names(instance))
 rat <- instance[vara] / instance[varb]
 if (any(is.infinite(rat))) warning(paste("Infinite values in ratio " , vara, varb ))
 rat
}

#'  Requires max path distance as well. That is, it works on summarized data.
#'  @export
compute_derived <- function(cell_vars) {
  newvars <- numeric()
  newvars[['ratio_x']] <- ratio(cell_vars, 'width' , 'x_sd')
  newvars[['ratio_y']] <- ratio(cell_vars, 'height' , 'y_sd')
  newvars[['density_bifs']] <- ratio(cell_vars, 'N_bifurcations' , 'total_length')
  newvars[['density_dist']] <-  ratio(cell_vars, 'node_root_path.max' , 'total_length')
  newvars[['aspect_ratio']] <-  ratio(cell_vars, 'dims.PC1' , 'dims.PC2')
  # not currently computed i think
  # newvars[['density_area']] <-  ratio(cell_vars, 'grid_area' , 'total_length')
  newvars[['tree_length.avg']] <- ratio(cell_vars, 'total_length' , 'N_stems')
  # check none of new names already in instance
  stopifnot(!any(names(newvars) %in% names(cell_vars)))
  c(cell_vars, newvars)
}

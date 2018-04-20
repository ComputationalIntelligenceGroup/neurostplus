#' Petilla variables applicable to any domain that do not require laminar data.
#'
#' @export
quantify_petilla_neurtral <- function(branch_node) {
  short <- count_short_vertical_terminals(branch_node)
  horizontal <- horizontal_growth(branch_node)
  c(short, horizontal)
}#
count_short_vertical_terminals <- function(cell) {
  stopifnot(is_unique_neuron(cell), is_unique_neurite_type(cell))
  pret <- subset(cell, is_pre_terminal)$branch
  if (length(pret) == 0) return(NA)
  pret <- unique(pret)
  count <- sum(sapply(pret, count_b_sv_term, cell))
  c(short_vertical_terminals  = count )
}
horizontal_growth <- function(neuron) {
  stopifnot(is_unique_neuron(neuron), is_unique_neurite_type(neuron))
  cxo <- sum(abs(neuron$change_x))
  cx <- sum(sign(neuron$x)* neuron$change_x)
  cyo <- sum(abs(neuron$change_y))
  c(gx = cx, gxo = cxo, gyo = cyo, horizontal_growth = cx / cxo)
}
is_short_vert <- function(term, parent) {
 dy <- abs(parent['y'] - term['y'])
 dx <- abs(parent['x'] - term['x'])
 unname(dy < 50 && dx < (dy / 2))
}
count_b_sv_term <- function(branch_id, cell) {
  child <- paste0(branch_id, '-', 1:2)
  tips <- subset(cell, branch %in% child & is_terminal)

  preter <- subset(cell, branch == branch_id & is_pre_terminal)[, c('x', 'y', 'z')]
  preter <- as.matrix(preter)[1, ]
  tips <- as.matrix(tips[, c('x', 'y', 'z')])
  sum(apply(tips, 1, is_short_vert, preter ))
}
#' Estimate whether the axon originates above or below the soma
compute_origin <- function(cell) {
  stopifnot(is_unique_neuron(cell))
  axon <- filter_neurite(cell)
  rm(cell)
  return <- c(axon_origin = NA, axon_above_below = NA)
  init <- unlist(subset(axon, is_initial)[1, 'y'])
  if (length(init) >  0) {
    sube <- subset(axon, x > 100)
    suppressWarnings(above <- min(sube$path_dist))
    if (is.infinite(above)) above <- 0
    sube <- subset(axon, x < -100)
    suppressWarnings(below <- min(sube$path_dist))
    if (is.infinite(below)) below <- 0
    return[] <- c(init, below - above)
  }
  return (return)
}

compute_upper_x <- function(cell) {
  if (!'N_descs' %in% names(cell)) {
      stop("Must be a branch primitive and have N_descs ")
  }
  stopifnot(is_unique_neuron(cell))
  axon <- filter_neurite(cell)
  rm(cell)
  maxy <- max(axon$y)
  # todo: pass thickness as parameter
  miny <- maxy - 165
  axon <- subset(axon, y >= miny & y <= maxy)
  vars <- numeric()
  vars['bifs'] <- nrow(subset(axon, N_descs > 1))
  vars['width'] <- diff(range(axon$x))
  vars <- c(vars, horizontal_growth(axon))
  vars
}

#' MC upper part specific width
#'
#' Considers symmetry in X (mean X), width in X, and the growth direction (mainly vertical or horizontal)
#' Note: for growth direction, the parent (or child) of segment may not be in the upper 165 part, but not very important
compute_mc_l1 <- function(branch_node, layer, thickness_mean, thickness_sd) {
  l1 <- prob_l1(branch_node, layer, thickness_mean, thickness_sd)
  vars <- compute_upper_x(branch_node)
  vars <- l1 * vars
  names(vars) <- paste('l1_', names(vars), sep = "")
  vars
}

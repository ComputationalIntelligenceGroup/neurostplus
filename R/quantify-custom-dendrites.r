compute_dendritic_polarity <- function(node) {
  stopifnot(is_unique_neuron(node))
  dendrite <- filter_neurite(node, axon = FALSE)
  rm(node)

  points <- get_insertion_points(dendrite)
  grouped  <- dplyr::group_by(dendrite, neurite)
  lengths <- dplyr::summarise(grouped, dendrite_length  = sum(node_length))
  # match lengths to points
  points_lengths <- dplyr::left_join(lengths, points, by = "neurite")
  points_lengths <- as.data.frame(points_lengths)
  a <- replicate_insertion_points(points_lengths$dendrite_length, points_lengths)

  eccs <- compute_pca_vars(a)[c('eccentricity', 'radial')]
  c(insert = eccs)
}
get_insertion_points <- function(node) {
  stopifnot(is_unique_neuron(node))
  dendrite <- filter_neurite(node, axon = FALSE)
  xy <- subset(dendrite, branch == 1 & node_order == 0)
  stopifnot(nrow(xy) == count_stems(dendrite) )
  xy
}
#' Replicate each point according to the length of its dendrite
replicate_insertion_points <-  function(lens, points) {
  lens <- as.integer(lens)
  reps <- lapply(seq_along(lens), function(i) points[rep(i, lens[i]), ])
  a <- Reduce(rbind, reps)
  stopifnot(nrow(a) == sum(lens))
  a
}

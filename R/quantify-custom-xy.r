# TODO: Z should be ignored in all of these functions?

#' Computes moments and range of XYZ
#' @export
compute_bounds_moments <- function(xyz) {
  ranges <- apply(xyz, 2, range)
  ranges <- apply(ranges, 2, diff)
  vars <- setNames(ranges, nm = c('width', 'height', 'depth'))
  moments <- lapply(xyz, summarize_moments)
  moments <- unlist(moments)
  names(moments) <- gsub('\\.', '_', names(moments) )
  c(vars, moments)
}
summarize_moments <- function(x) {
    c(mean = mean(x), sd = sd(x), mean_abs = mean(abs(x)), std_mean = mean(x) / sd(x), kurtosis = moments::kurtosis(x),
      skew = moments::skewness(x), min = min(x), max=max(x), min_abs  = min(abs(x)), max_abs = max(abs(x)))
}
#' Computes center of mass.
#' assume soma is at 0, 0, 0.
#' Should also have soma as input and then check this.
compute_com <- function(xyz) {
  com <- colSums(xyz) / nrow(xyz)
  dist <- norm(com, type = "2")
  c(com, dist = dist)
}
#' Computes mean direction
#' Normalizes so that each segment is given equal importance. This loses some information, as segments have different length.
compute_mean_direction <- function(xyz) {
  overall <- colSums(norm_by_row(xyz)) / nrow(xyz)
  dist <- norm(overall, type = "2")
  normalized <- overall / dist
  sine <- normalized[2]
  # radial <- abs(normalized[1]) - abs(normalized[2])
  c(overall, dist = dist, sine = sine, vertical_direction = sine * dist)
}
#' Norm by row
norm_by_row <- function(xyz) {
  xyz / sqrt(rowSums(xyz  ^ 2 ))
}
#' Compute  pca vars
#'
#' sds, and axialized, 1st component direction
#'
#' s1 close to s2: spherical.
#' s1 much higher: ellipse in 2D.
#' It is also possible to use SDs as variables (Cannon)
#' axes: 1 is x and 2 is y
#'
#' radial: > 0 means radial; below 0 means going up
#' @export
compute_pca_vars <- function(xyz) {
  xyz <- filter_xyz(xyz)
  # only care about eccentricity in X and Y dimensions
  xyz[, 'z'] <- 0
  pr <- prcomp(xyz, scale. = FALSE)
  sdev <- pr$sdev
  rot <- pr$rotation
  x <- rot['x', 1]
  y <- rot['y', 1]
  z <- rot['z', 1]
  eccentricity <- 1 - (sdev[2] / sdev[1] )
  radial <- abs(y) - abs(x)
  radial <- radial * eccentricity
  dims <- apply(apply(pr$x, 2, range), 2, diff)
  # axes <- dims / 2
  # eccentricity_farsight <- sqrt(1 - (axes[2] ^ 2 / axes[1] ^ 2))
  c(sdev = sdev, eccentricity = eccentricity, radial = radial, pcx = x, pcy = y, pcz = z,
    dims = dims, box_area = unname(dims[1] * dims[2]))
}

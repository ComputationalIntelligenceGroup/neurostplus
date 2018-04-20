#' compute_primitives_branch
#' @export
compute_primitives_branch <- function(path) {
  branch <- neurostr::json2dataframe(neurostr::compute_branch_features(path))
  # todo: remove this function and fix it in neurostr c++
  branch <- flag_branches(branch)
  branch
}


#' compute_primitives_node
#' @export
compute_primitives_node <- function(path) {
  node <- neurostr::json2dataframe(neurostr::compute_node_features(path))
  node
}

#' compute_primitives_branch_node
#' @export
compute_primitives_branch_node <- function(path) {
  jsonb <- compute_primitives_branch(path)
  jsonn <- compute_primitives_node(path)
  neurostr::merge_branch_node(jsonb, jsonn)
}

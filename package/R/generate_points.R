#' Generate new points from density matrix
#'
#' @param density A matrix with density values.
#' @param num_points The number of points to sample
#'
#' @export
generate_points <- function(density, num_points = 1000) {
  # perform weighted sampling. replace is TRUE so that it is more efficient.
  ix <- sample.int(length(density), num_points, prob = density, replace = TRUE)

  # get indices of sampled positions
  pts <- data.frame(arrayInd(ix, dim(density)))
  colnames(pts) <- c("x", "y")

  # scale position indices
  pts %>% mutate(x = x / ncol(density), y = y / nrow(density))
}

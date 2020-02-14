#' Read trajectory svg
#'
#' @param id The name of the trajectory svg
#'
#' @export
#'
#' @importFrom workspace raw_file derived_file
#' @importFrom png readPNG
read_trajectory_svg <- function(id) {
  raw_file_root <- workspace::raw_file(id, experiment_id = "trajectory_svgs")
  svg_file <- paste0(raw_file_root, ".svg")

  derived_file_root <- workspace::derived_file(id, experiment_id = "trajectory_svgs")
  pdf_file <- paste0(derived_file_root, ".pdf")
  png_file <- paste0(derived_file_root, ".png")

  # convert svg to pdf
  system(paste0("inkscape '", svg_file, "' --export-pdf='", pdf_file, "'"))

  # convert pdf to png
  system(paste0("pdftoppm -scale-to 1000 -png '", pdf_file, "' -singlefile '", derived_file_root, "'"))

  # read image from png
  traj_png <- png::readPNG(png_file)

  # collect gold points from green channel
  gold <- data.frame(
    which(traj_png[, , 2] != 0, arr.ind = T)
  )[,c(2,1)]
  colnames(gold) <- c("x", "y")

  # collect density grid from red channel
  density <- t(traj_png[, , 1])

  # scale gold coordinates
  gold <- gold %>% mutate(x = x / ncol(density), y = y / nrow(density))

  # return output
  list(
    gold = gold,
    density = density
  )
}

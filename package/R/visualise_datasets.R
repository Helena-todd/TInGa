#' visualise datasets
#'
#' @param files the files to be visualised, as passed to the dynbenchmark::load_dataset() function
#' @param directory the directory where the pngs should be saved
#'
#' @return The plots for all files
#' @export
#'
#' @examples
#' visualise_datasets(files = c("synthetic/dyntoy/cyclic_1","synthetic/dyntoy/bifurcating_4"),
#'                    directory = "plots/")
visualise_datasets <- function(files, directory){
  dir.create(directory)

  pbapply::pblapply(files, function(file) {
    print(file)
    dataset <- dynbenchmark::load_dataset(file)

    g1 <- plot_graph(dataset)
    g2 <- plot_heatmap(dataset %>% add_root(), features_oi = 100)

    dimred <- dyndimred::dimred_landmark_mds(get_expression(dataset), num_landmarks = 1000)
    g3 <- plot_dimred(dataset, dimred = dimred)

    g4 <- plot_dimred(dataset, dimred = dyndimred::dimred_ica)

    pl <- patchwork::wrap_elements(g1 + g2 + g3 + g4) + ggtitle(glue::glue("ID = {dataset$id}; trajectory type = {dataset$trajectory_type}"))

    plot_path <- dataset$id %>% str_replace_all("/", "_") %>% paste0(directory, ., ".png")
    ggsave(plot_path, pl, width = 10, height = 10, dpi = 100)

    return()
  })
}





plot_dist_circles <- function(x, mean_knn_dist){
  xdf <- data.frame(x, mean_knn_dist) %>% as_tibble()
  p <- ggplot(xdf, aes(x0 = comp_1, y0 = comp_2, r = mean_knn_dist)) +
    ggforce::geom_circle()
  print(p)
}






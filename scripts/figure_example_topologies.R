datasets <- dynbenchmark::list_datasets()$id
load("derived_data/dataset_info_with_nb_cells.RData")

dta_info <- dataset_info %>%
  column_to_rownames("id")
dta_info <- dta_info[datasets,]

toplot <- which(dta_info$nb_cells < 3000)

pdf("~/Desktop/datasets_ex.pdf")
 for (idx in toplot){
   print(idx)
   dta <- dynbenchmark::load_dataset(datasets[idx])
   p <- plot_dimred(dta)
   plot(p)
 }
dev.off()


dta_indices <- toplot[c(17, 144, 150, 191, 164, 170, 180, 215, 184)]
dta_types <- dta_info$trajectory_type[toplot[c(17, 144, 150, 191, 164, 170, 180, 215, 184)]]
plots <- lapply(seq_along(dta_indices), function(i){
  dta <- dynbenchmark::load_dataset(datasets[dta_indices[i]])
  p <- plot_dimred(dta) + ggtitle(dta_types[i]) +
    theme(axis.title.x=element_blank(),
          axis.title.y=element_blank(),
          text = element_text(size=18))
  p
})

gridExtra::grid.arrange(plots[[1]], plots[[2]], plots[[3]],
                        plots[[4]], plots[[5]], plots[[6]],
                        plots[[7]], plots[[8]], plots[[9]],
                        nrow = 3)

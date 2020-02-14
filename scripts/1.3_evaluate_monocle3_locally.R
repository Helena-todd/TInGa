library(tidyverse)
library(workspace)
library(gng)
library(dyno)
library(ggplot2)
library(researchgng)
library(qsub)
library(RColorBrewer)
library(gridExtra)
library(dynbenchmark)

datasets <- dynbenchmark::list_datasets()$id

# small tweaks for some datasets names
datasets_other_names <- datasets
datasets_other_names[166:176] <- gsub("synthetic/dyngen/koen/", "synthetic/dyngen/koen_", datasets[166:176])
datasets_other_names[297:315] <- gsub("synthetic/prosstt/", "synthetic/prosstt_", datasets[297:315])
datasets_other_names[316:350] <- gsub("synthetic/splatter/", "synthetic/splatter_", datasets[316:350])

methods <-
  list(
    "monocle_3" = monocle3_param()
  )

combi2 <- crossing(
  methods = names(methods),
  dataset = datasets[c(166:176, 297:315, 316:350)]
)

combi2$other_name <- datasets_other_names[c(166:176, 297:315, 316:350)]

res1 <- lapply(seq_len(nrow(combi2)), function(i){
  dta_name <- strsplit(combi2$other_name[i], "/")
  filename <- paste0(combi2$methods[i], "_", dta_name[[1]][length(dta_name[[1]])])
  existing_files <- list.files("results/eval_dynbenchmark/monocle3/")
  if (!paste0(filename, ".RDS") %in% existing_files){
    tryCatch({
      params <- combi2[i,]
      dataset <- dynbenchmark::load_dataset(params$dataset)

      eval <- dyneval::evaluate_ti_method(
        dataset = dataset,
        method = researchgng::monocle3_param(),
        parameters = NULL,
        metrics = c("correlation", "him", "featureimp_wcor", "F1_branches")
      )
      metrics <- eval$summary
      mean_score <- dynutils::calculate_geometric_mean(metrics$correlation,metrics$him,
                                                       metrics$featureimp_wcor,
                                                       metrics$F1_branches)
      names(mean_score) <- "geom_mean_score"
      eval$summary <- cbind(metrics, params, mean_score)
      eval
      saveRDS(eval, file = paste0("results/eval_dynbenchmark/monocle3/", filename, ".RDS"))
      eval
    }, error = function(e) {
      e$msg
      # e$message
    })
  }
})

saveRDS(res1, file = "results/eval_dynbenchmark/eval_monocle3_missing_datasets.RDS")






################################################################################
##############          RECOVERING     THE      RESULTS          ###############
################################################################################

existing_files <- list.files("results/eval_dynbenchmark/monocle3/")
setwd("results/eval_dynbenchmark/monocle3/")

output2 <- list()

for(idx in seq_along(datasets)){
  dta_name <- strsplit(datasets_other_names[[idx]], "/")
  filename <- paste0("monocle_3", "_", dta_name[[1]][length(dta_name[[1]])], ".RDS")
  if(filename %in% existing_files){
    output <- readRDS(filename)
    output2[[idx]] <- output
  } else {
    output <- list(summary = list(),
                   models = NULL)
    output$summary$error <- "unknown error"
    output$summary$method_id <- "monocle3"
    output$summary$dataset_id <- datasets[idx]
    output$summary$methods <- "Monocle3"
    output$summary$him <- 0
    output$summary$correlation <- 0
    output$summary$F1_branches <- 0
    output$summary$featureimp_wcor <- 0
    output$summary$mean_score <- 0
    output$summary$time_method <- NA

    output2[[idx]] <- output
  }
}

save(output2, file = "../eval_monocle3_alldtasets.RData")


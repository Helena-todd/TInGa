library(ggplot2)
library(dplyr)

# Statistical tests to compare methods

load("results/eval_dynbenchmark/for_paper_eval_and_output3.RData")
load("derived_data/dataset_info_with_nb_cells.RData")

load("results/eval_dynbenchmark/datasets_split.RData")
datasets_test <- split_datasets$datasets_test

# datasets <- dynbenchmark::list_datasets()$id
# test_idx <- sample(1:350, size = 250, replace = FALSE)
# datasets_test <- datasets[test_idx]
# datasets_train <- datasets[-test_idx]
# split_datasets <- lst(datasets_test, datasets_train)
# save(split_datasets, file = "results/eval_dynbenchmark/datasets_split.RData")
all_eval <- list_results$all_eval
all_eval <- all_eval %>%
  filter(dataset_id %in% datasets_test)
all_eval <- all_eval[which(all_eval$method_id %in% c("gng_param2", "slingshot", "monocle_3", "paga",
                                                     "raceid_stemid")),]
all_eval$method_id[which(all_eval$method_id=="gng_param2")] <- "tinga"
all_eval$method_id <- factor(all_eval$method_id,
                             levels = c("tinga", "monocle_3", "slingshot", "paga",
                                        "raceid_stemid"))

traj_type <- dataset_info[,c("id", "trajectory_type")] %>%
  column_to_rownames("id")
traj_types2 <- traj_type[all_eval$dataset_id,]
all_eval2 <- all_eval %>%
  mutate(trajectory_type = traj_types2)

pvalues <- lapply(names(table(all_eval2$trajectory_type)), function(traj){
  dta_tmp <- all_eval2 %>%
    filter(trajectory_type == traj)
  p_vals <- c()
  for (oth in levels(all_eval$method_id)[-1]){
    tinga <- dta_tmp$mean_score[which(dta_tmp$method_id == "tinga")]
    other <- dta_tmp$mean_score[which(dta_tmp$method_id == oth)]
    res <- t.test(tinga, other, "greater")$p.value
    p_vals <- c(p_vals, round(res, digits = 4))
  }
  p_vals
})

pvals <- do.call(rbind.data.frame, pvalues)
colnames(pvals) <- levels(all_eval$method_id)[-1]
rownames(pvals) <- names(table(all_eval2$trajectory_type))

tt1 <- ttheme_default()
tab <- tableGrob(pvals, rows=rownames(pvals), theme=tt1)
plot(tab)

library(tidyverse)
library(workspace)
library(dyno)
library(ggplot2)
library(researchgng)
library(qsub)
library(RColorBrewer)
library(gridExtra)
library(dynbenchmark)
library(furrr)

project("researchgng", "3.6_merge_eval_results")

datasets <- dynbenchmark::list_datasets()$id

# missing_datasets <- as.data.frame(datasets[166:176]) %>%
#   magrittr::set_colnames("id")
# missing_datasets$source <- rep("synthetic/dyngen/", nrow(missing_datasets))
# missing_datasets$creation_date <- rep("NA", nrow(missing_datasets))
# missing_datasets$trajectory_type <- rep("cycle", nrow(missing_datasets))
# missing_datasets$directed <- rep("FALSE", nrow(missing_datasets))
# missing_datasets$root_milestone_id <- rep("NA", nrow(missing_datasets))
# missing_datasets$standard <- rep("NA", nrow(missing_datasets))
# missing_datasets$task_source <- rep("NA", nrow(missing_datasets))
# missing_datasets$model <- rep("Koen", nrow(missing_datasets))
#
# missing_dataset_size <- lapply(seq_len(nrow(missing_datasets)), function(dta_idx){
#   dta <- missing_datasets$id[dta_idx]
#   dtaset <- dynbenchmark::load_dataset(dta)
#   nrow(dtaset$expression())
# })
#
# missing_datasets$nb_cells <- unlist(missing_dataset_size)
# dataset_info <- rbind(dataset_info, missing_datasets)
#
# save(dataset_info, file = "derived_data/dataset_info_with_nb_cells.RData")
load("derived_data/dataset_info_with_nb_cells.RData")

################    Fetch back results of PAGA and slingshot    ################
load("results/eval_dynbenchmark/eval_gng_top_ti_methods_alldtasets.RData")

# output_all <- output2
ix <- map_lgl(output2, ~length(.) != 1 && !is.na(.)) %>% which()
output_all <- output2[ix]
eval <- output2[ix] %>% map_df(~.$summary) %>% mutate(ix = ix)
outputs_of_interest <- which(eval$methods %in% c("paga", "slingshot"))
output_1 <- output_all[outputs_of_interest]

eval1 <- eval %>%
  filter(methods %in% c("paga", "slingshot"))

eval1_res <- eval1 %>%
  select(method_id, dataset_id, mean_score, him, F1_branches, correlation, featureimp_wcor, time_method)


#############    Fetch back results of GNG, PAGA tree and raceID    #############
load("results/eval_dynbenchmark/eval_gng_pagatree_stemid_alldatasets.RData")

ix <- map_lgl(output2, ~length(.) != 1 && !is.na(.)) %>% which()
eval <- output2[ix] %>% map_df(~.$summary) %>% mutate(ix = ix)

output_all <- output2[ix]
outputs_of_interest <- which(eval$method_id %in% c("raceid_stemid"))
output_2 <- output_all[outputs_of_interest]

eval2 <- eval %>%
  filter(method_id %in% c("raceid_stemid"))

eval2_res <- eval2 %>%
  select(method_id, dataset_id, mean_score, him, F1_branches, correlation, featureimp_wcor, time_method)



#################       Fetch back results of MONOCLE 3       #################
load("results/eval_dynbenchmark/eval_monocle3_alldtasets.RData")

ix <- map_lgl(output2, ~length(.) != 1 && !is.na(.)) %>% which()
eval <- output2[ix] %>% map_df(~.$summary) %>% mutate(ix = ix)

output_all <- output2[ix]
output_3 <- output_all

eval3_res <- eval %>%
  mutate(method_id = methods) %>%
  select(method_id, dataset_id, mean_score, him, F1_branches, correlation, featureimp_wcor, time_method)


#################       Fetch back results of GNG_post_process       #################
load("results/eval_dynbenchmark/eval_gng_post_process_alldatasets2.RData")

ix <- map_lgl(output2, ~length(.) != 1 && !is.na(.)) %>% which()
eval <- output2[ix] %>% map_df(~.$summary) %>% mutate(ix = ix)

output_all <- output2[ix]
output_4 <- output_all

eval4_res <- eval %>%
  select(method_id, dataset_id, mean_score, him, F1_branches, correlation, featureimp_wcor, time_method)



all_eval <- rbind(eval1_res, eval2_res, eval3_res, eval4_res)
all_output <- c(output_1, output_2, output_3, output_4)

list_results <- lst(all_eval = all_eval,
                    all_output = all_output)
save(list_results, file = "results/eval_dynbenchmark/for_paper_eval_and_output3.RData")
load("results/eval_dynbenchmark/all_eval_and_output.RData")

all_eval <- list_results$all_eval

all_eval$method_id[which(all_eval$method_id == "gng_param2")] <- "gng"
all_eval$method_id <- factor(all_eval$method_id,
                                 levels = c("gng", "monocle_3", "slingshot", "paga",
                                            "raceid_stemid", "paga_tree"))

ggplot(all_eval, aes(x = method_id, y = mean_score, color = method_id)) +
  geom_boxplot(outlier.size = 0.1) +
  # scale_y_log10()+
  # geom_jitter(outlier.size = 0.1) +
  theme(axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        text = element_text(size=18))



dta_size <- lapply(seq_len(nrow(all_eval)), function(row_idx){
  dataset_name <- all_eval[row_idx, "dataset_id"]
  dataset_info$nb_cells[which(dataset_info$id == dataset_name)]
})

all_eval$data_size <- unlist(dta_size)



ggplot(all_eval, aes(x = data_size, y = time_method)) +
  geom_line(aes(color = method_id))



eval_tmp <- all_eval[,1:3] %>%
  filter(method_id != "gng_param")
eval_tmp <- split(eval_tmp, eval_tmp$method_id)
dataset_ids <- lapply(eval_tmp, function(l){
  l$dataset_id
})

common_ids <- Reduce(intersect,dataset_ids)

##### plot all results for the 36 disconnected datasets

disco <- dataset_info$id[which(dataset_info$trajectory_type == "disconnected_graph")]
which(common_ids %in% disco)

pdf("~/Desktop/disconnected_datasets.pdf", width = 12, height = 8)
for(idx in which(common_ids %in% disco)){
  dta <- common_ids[idx]
  print(paste0("Printing figures for dataset ", idx))
  of_int <- which((all_eval$method_id == "paga")&(all_eval$dataset_id == dta))
  if (!is.null(all_output[[of_int]]$models[[1]])){
    # some NaN are generated -> I remove them
    #all_output[[of_int]]$models[[1]]$dimred_segment_points <- all_output[[of_int]]$models[[1]]$dimred_segment_points[!is.na(all_output[[of_int]]$models[[1]]$dimred_segment_progressions$percentage),]

    # all_output[[of_int]]$models[[1]]$dimred_segment_progressions <- all_output[[of_int]]$models[[1]]$dimred_segment_progressions[!is.na(all_output[[of_int]]$models[[1]]$dimred_segment_progressions$percentage),]
    all_output[[of_int]]$models[[1]]$dimred_segment_progressions[is.na(all_output[[of_int]]$models[[1]]$dimred_segment_progressions$percentage),3] <- 0

    p_paga <- plot_dimred(all_output[[of_int]]$models[[1]]) +
      ggtitle(paste0("PAGA, mean_score = ", all_eval[of_int, "mean_score"])) +
      theme(plot.title = element_text(size = 8, face = "bold"))
    all_eval_paga <- all_eval[of_int,]
  } else {
    p_paga <-ggplot() + geom_point() +
      ggtitle("PAGA, mean_score = 0")+
      theme(plot.title = element_text(size = 8, face = "bold"))
  }

  of_int <- which((all_eval$method_id == "paga_tree")&(all_eval$dataset_id == dta))
  if (!is.null(all_output[[of_int]]$models[[1]])){
    p_paga_tree <- plot_dimred(all_output[[of_int]]$models[[1]]) +
      ggtitle(paste0("PAGA TREE, mean_score = ", all_eval[of_int, "mean_score"]))+
      theme(plot.title = element_text(size = 8, face = "bold"))
    all_eval_paga_tree <- all_eval[of_int,]
  } else {
    p_paga_tree <- ggplot() + geom_point() +
      ggtitle("PAGA TREE, mean_score = 0")+
      theme(plot.title = element_text(size = 8, face = "bold"))
  }


  of_int <- which((all_eval$method_id == "slingshot")&(all_eval$dataset_id == dta))
  if (!is.null(all_output[[of_int]]$models[[1]])){
    p_slingshot <- plot_dimred(all_output[[of_int]]$models[[1]]) +
      ggtitle(paste0("SLINGSHOT, mean_score = ", all_eval[of_int, "mean_score"]))+
      theme(plot.title = element_text(size = 8, face = "bold"))
    all_eval_slingshot <- all_eval[of_int,]
  } else {
    p_slingshot <- ggplot() + geom_point() +
      ggtitle("SLINGSHOT, mean_score = 0")+
      theme(plot.title = element_text(size = 8, face = "bold"))
  }

  of_int <- which((all_eval$method_id == "raceid_stemid")&(all_eval$dataset_id == dta))
  if (!is.null(all_output[[of_int]]$models[[1]])){
    p_raceid <- plot_dimred(all_output[[of_int]]$models[[1]]) +
      ggtitle(paste0("RACE ID, mean_score = ", all_eval[of_int, "mean_score"]))+
      theme(plot.title = element_text(size = 8, face = "bold"))
    all_eval_race_id <- all_eval[of_int,]
  } else {
    p_raceid <- ggplot() + geom_point() +
      ggtitle("RACE ID, mean_score = 0") +
      theme(plot.title = element_text(size = 8, face = "bold"))
  }

  of_int <- which((all_eval$method_id == "gng_param2")&(all_eval$dataset_id == dta))
  if (!is.null(all_output[[of_int]]$models[[1]])){
    p_gng <- plot_dimred(all_output[[of_int]]$models[[1]]) +
      ggtitle(paste0("GNG, mean_score = ", all_eval[of_int, "mean_score"]))+
      theme(plot.title = element_text(size = 8, face = "bold"))
    all_eval_gng <- all_eval[of_int,]
  } else {
    p_gng <- ggplot() + geom_point() +
      ggtitle("GNG, mean_score = 0")+
      theme(plot.title = element_text(size = 8, face = "bold"))
  }

  of_int <- which((all_eval$method_id == "monocle_3")&(all_eval$dataset_id == dta))
  if (!is.null(all_output[[of_int]]$models[[1]])){
    p_mon <- plot_dimred(all_output[[of_int]]$models[[1]]) +
      ggtitle(paste0("MONOCLE 3, mean_score = ", all_eval[of_int, "mean_score"]))+
      theme(plot.title = element_text(size = 8, face = "bold"))
    all_eval_monocle <- all_eval[of_int,]
  } else {
    p_mon <- ggplot() + geom_point() +
      ggtitle("Monocle_3, mean_score = 0")+
      theme(plot.title = element_text(size = 8, face = "bold"))
  }

  dta_tmp <- dynbenchmark::load_dataset(dta)
  p_gold <- plot_dimred(dta_tmp) + ggtitle(dta)+
    theme(plot.title = element_text(size = 8, face = "bold"))

  all_eval_tmp <- rbind(all_eval_gng, all_eval_race_id, all_eval_paga,
                        all_eval_paga_tree, all_eval_slingshot, all_eval_monocle)
  # df <- all_eval_tmp %>%
  #   dplyr::select(-dataset_id, -data_size) %>%
  #   gather(key = c("mean_score", "correlation", "him", "F1_branches", "featureimp_wcor"), value = "value", -method_id)
  # head(df)

  grid.arrange(p_gold, p_gng, p_raceid, p_paga, p_paga_tree, p_slingshot, p_mon, nrow = 2)

}
dev.off()



#### plot only results where gng is better

to_try <- c()
to_try_res <- list()
for (i in seq_along(datasets)){
  dta <- datasets[i]
  all_tp <- all_eval %>% filter(dataset_id == dta)
  if(all_tp$method_id[which.max(all_tp$mean_score)] == "gng_param2"){
    #print(paste0("good try: ", i))
    to_try <- c(to_try, i)
    to_try_res[[i]] <- all_tp
  }
}
to_try_res[[16]]

pdf("~/Desktop/good_gng_datasets.pdf", width = 12, height = 8)
for(idx in to_try){
  res <- to_try_res[[idx]]
  of_int <- which((all_eval$method_id == "paga")&(all_eval$dataset_id == res$dataset_id[1]))
  if (!is.null(output_all[[of_int]]$models[[1]])){
    p_paga <- plot_dimred(output_all[[of_int]]$models[[1]]) +
      ggtitle("PAGA")
  } else {
    p_paga <-ggplot() + geom_point() +
      ggtitle("PAGA")
  }

  of_int <- which((all_eval$method_id == "paga_tree")&(all_eval$dataset_id == res$dataset_id[1]))
  if (!is.null(output_all[[of_int]]$models[[1]])){
    p_paga_tree <- plot_dimred(output_all[[of_int]]$models[[1]]) +
      ggtitle("PAGA TREE")
  } else {
    p_paga_tree <- ggplot() + geom_point() +
      ggtitle("PAGA TREE")
  }


  of_int <- which((all_eval$method_id == "slingshot")&(all_eval$dataset_id == res$dataset_id[1]))
  if (!is.null(output_all[[of_int]]$models[[1]])){
    p_slingshot <- plot_dimred(output_all[[of_int]]$models[[1]]) +
      ggtitle("SLINGSHOT")
  } else {
    p_slingshot <- ggplot() + geom_point() +
      ggtitle("SLINGSHOT")
  }

  of_int <- which((all_eval$method_id == "raceid_stemid")&(all_eval$dataset_id == res$dataset_id[1]))
  if (!is.null(output_all[[of_int]]$models[[1]])){
    p_raceid <- plot_dimred(output_all[[of_int]]$models[[1]]) +
      ggtitle("RACE ID")
  } else {
    p_raceid <- ggplot() + geom_point() +
      ggtitle("RACE ID")
  }

  of_int <- which((all_eval$method_id == "gng_param_init")&(all_eval$dataset_id == res$dataset_id[1]))
  if (!is.null(output_all[[of_int]]$models[[1]])){
    p_gng <- plot_dimred(output_all[[of_int]]$models[[1]]) +
      ggtitle("GNG")
  } else {
    p_gng <- ggplot() + geom_point() +
      ggtitle("GNG")
  }

  dta_tmp <- dynbenchmark::load_dataset(res$dataset_id[1])
  p_gold <- plot_dimred(dta_tmp) + ggtitle("Gold standard")

  grid.arrange(p_gold, p_gng, p_raceid, p_paga, p_paga_tree, p_slingshot, nrow = 2)

}
dev.off()



#####################################################
#### GENERATE OUTPUT FOR GENERATE STATISTICS FUNCTION

output_2 <- lapply(output_2, function(x){
  x$summary$method_id <- x$summary$methods
  x
})

output_all <- c(output_1, output_2)

output2 <- all_output

saveRDS(output2, file = "derived_data/output_for_paper_for_stats.RDS")


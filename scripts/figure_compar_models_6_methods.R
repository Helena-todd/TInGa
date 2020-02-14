library(gridExtra)
library(tidyverse)
library(dynplot)

# Identify, for each dataset, which method performed best, and make a figure
# out of it

load("results/eval_dynbenchmark/all_eval_and_output.RData")
all_eval <- list_results$all_eval
all_output <- list_results$all_output[which(all_eval$method_id %in% c("gng_param2", "slingshot", "monocle_3", "paga",
                                                                      "paga_tree", "raceid_stemid"))]
all_eval <- all_eval[which(all_eval$method_id %in% c("gng_param2", "slingshot", "monocle_3", "paga",
                                                     "paga_tree", "raceid_stemid")),]

load("results/eval_dynbenchmark/datasets_split.RData")
datasets_test <- split_datasets$datasets_test

eval_tmp <- all_eval[,1:3] %>%
  filter(method_id != "gng_param") %>%
  filter(dataset_id %in% datasets_test)
eval_tmp <- split(eval_tmp, eval_tmp$method_id)
dataset_ids <- lapply(eval_tmp, function(l){
  l$dataset_id
})

common_ids <- Reduce(intersect,dataset_ids)

paga_fails <- c()
paga_tree_fails <- c()
slingshot_fails <- c()
raceid_stemid_fails <- c()
gng_param2_fails <- c()
monocle3_fails <- c()

for(i in seq_along(common_ids)){
  dta <- common_ids[i]
  of_int <- which((all_eval$method_id == "paga")&(all_eval$dataset_id == dta))
  if (is.null(all_output[[of_int]]$models[[1]])){
    paga_fails <- c(paga_fails, i)
  }
  of_int <- which((all_eval$method_id == "paga_tree")&(all_eval$dataset_id == dta))
  if (is.null(all_output[[of_int]]$models[[1]])){
    paga_tree_fails <- c(paga_tree_fails, i)
  }
  of_int <- which((all_eval$method_id == "slingshot")&(all_eval$dataset_id == dta))
  if (is.null(all_output[[of_int]]$models[[1]])){
    slingshot_fails <- c(slingshot_fails, i)
  }
  of_int <- which((all_eval$method_id == "raceid_stemid")&(all_eval$dataset_id == dta))
  if (is.null(all_output[[of_int]]$models[[1]])){
    raceid_stemid_fails <- c(raceid_stemid_fails, i)
  }
  of_int <- which((all_eval$method_id == "gng_param2")&(all_eval$dataset_id == dta))
  if (is.null(all_output[[of_int]]$models[[1]])){
    gng_param2_fails <- c(gng_param2_fails, i)
  }
  of_int <- which((all_eval$method_id == "monocle_3")&(all_eval$dataset_id == dta))
  if (is.null(all_output[[of_int]]$models[[1]])){
    monocle3_fails <- c(monocle3_fails, i)
  }
}

##### plot all results for the 350 datasets

# real/gold/mesoderm−development_loh (11)
# real/gold/stimulated−dendritic−cells−LPS_shalek
# real/silver/mouse−cell−atlas−combination−1 # disco
# real/silver/neonatal−rib−cartilage_mca # bifurc?
# real/silver/kidney−distal−convoluted−tubule_mca (29) linear
# real/silver/mouse−cell−atlas−combination−1 (31) disco
# real/silver/neonatal−rib−cartilage_mca (40) bifurc
# real/silver/thymus−t−cell−differentiation_mca (58) linear


pdf("~/Desktop/test_datasets.pdf", width = 12, height = 8)
for(idx in seq_along(common_ids)){
  dta <- common_ids[idx]
  print(paste0("Printing figures for dataset ", idx))

  dta_tmp <- dynbenchmark::load_dataset(dta)
  p_gold <- plot_dimred(dta_tmp) + ggtitle(dta)+
    theme(plot.title = element_text(size = 8, face = "bold"))
  dta_tmp$dimred <- dyndimred::dimred(dta_tmp$expression(), method = "landmark_mds", ndim = 2)
  p_gold <- plot_dimred(dta_tmp, dimred = dta_tmp$dimred) + ggtitle(dta)+
    theme(plot.title = element_text(size = 8, face = "bold"))

  of_int <- which((all_eval$method_id == "paga")&(all_eval$dataset_id == dta))
  if (!is.null(all_output[[of_int]]$models[[1]])){
    # some NaN are generated -> I remove them
    #all_output[[of_int]]$models[[1]]$dimred_segment_points <- all_output[[of_int]]$models[[1]]$dimred_segment_points[!is.na(all_output[[of_int]]$models[[1]]$dimred_segment_progressions$percentage),]

    # all_output[[of_int]]$models[[1]]$dimred_segment_progressions <- all_output[[of_int]]$models[[1]]$dimred_segment_progressions[!is.na(all_output[[of_int]]$models[[1]]$dimred_segment_progressions$percentage),]
    all_output[[of_int]]$models[[1]]$dimred_segment_progressions[is.na(all_output[[of_int]]$models[[1]]$dimred_segment_progressions$percentage),3] <- 0

    p_paga <- plot_dimred(all_output[[of_int]]$models[[1]], dimred = dta_tmp$dimred) +
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
    p_paga_tree <- plot_dimred(all_output[[of_int]]$models[[1]], dimred = dta_tmp$dimred) +
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
    p_slingshot <- plot_dimred(all_output[[of_int]]$models[[1]], dimred = dta_tmp$dimred) +
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
    dr <- dta_tmp$dimred[rownames(all_output[[of_int]]$models[[1]]$dimred),]
    p_raceid <- plot_dimred(all_output[[of_int]]$models[[1]], dimred = dr) +
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
    p_gng <- plot_dimred(all_output[[of_int]]$models[[1]], dimred = dta_tmp$dimred) +
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
    p_mon <- plot_dimred(all_output[[of_int]]$models[[1]], dimred = dta_tmp$dimred) +
      ggtitle(paste0("MONOCLE 3, mean_score = ", all_eval[of_int, "mean_score"]))+
      theme(plot.title = element_text(size = 8, face = "bold"))
    all_eval_monocle <- all_eval[of_int,]
  } else {
    p_mon <- ggplot() + geom_point() +
      ggtitle("Monocle_3, mean_score = 0")+
      theme(plot.title = element_text(size = 8, face = "bold"))
  }

  all_eval_tmp <- rbind(all_eval_gng, all_eval_race_id, all_eval_paga,
                        all_eval_paga_tree, all_eval_slingshot, all_eval_monocle)
  # df <- all_eval_tmp %>%
  #   dplyr::select(-dataset_id, -data_size) %>%
  #   gather(key = c("mean_score", "correlation", "him", "F1_branches", "featureimp_wcor"), value = "value", -method_id)
  # head(df)

  grid.arrange(p_gold, p_gng, p_raceid, p_paga, p_paga_tree, p_slingshot, p_mon, nrow = 2)

}
dev.off()


iters <- c(29, 31, 40)
to_plot <- common_ids[iters]


figures <- lapply(seq_along(to_plot), function(idx){
  dta <- to_plot[idx]
  print(paste0("Printing figures for dataset ", idx))

  dta_tmp <- dynbenchmark::load_dataset(dta)
  dta_tmp$dimred <- dyndimred::dimred(dta_tmp$expression(), method = "landmark_mds", ndim = 2)
  p_gold <- plot_dimred(dta_tmp, dimred = dta_tmp$dimred, color_cells = "grouping",
                        grouping = dynwrap::group_onto_nearest_milestones(dta_tmp)) +
    theme(legend.position = "none") +
    ggtitle(dta)+
    theme(plot.title = element_text(size = 15, face = "bold"))

  of_int <- which((all_eval$method_id == "paga")&(all_eval$dataset_id == dta))
  if (!is.null(all_output[[of_int]]$models[[1]])){
    # some NaN are generated -> I remove them
    #all_output[[of_int]]$models[[1]]$dimred_segment_points <- all_output[[of_int]]$models[[1]]$dimred_segment_points[!is.na(all_output[[of_int]]$models[[1]]$dimred_segment_progressions$percentage),]

    # all_output[[of_int]]$models[[1]]$dimred_segment_progressions <- all_output[[of_int]]$models[[1]]$dimred_segment_progressions[!is.na(all_output[[of_int]]$models[[1]]$dimred_segment_progressions$percentage),]
    all_output[[of_int]]$models[[1]]$dimred_segment_progressions[is.na(all_output[[of_int]]$models[[1]]$dimred_segment_progressions$percentage),3] <- 0

    p_paga <- plot_dimred(all_output[[of_int]]$models[[1]], dimred = dta_tmp$dimred, color_cells = "grouping",
                          grouping = dynwrap::group_onto_nearest_milestones(dta_tmp)) +
      theme(legend.position = "none") +
      ggtitle(paste0("PAGA, mean_score = ", round(all_eval[of_int, "mean_score"], digits = 2))) +
      theme(plot.title = element_text(size = 15, face = "bold"))
    all_eval_paga <- all_eval[of_int,]
  } else {
    p_paga <-ggplot() + geom_point() +
      ggtitle("PAGA, mean_score = 0")+
      theme(plot.title = element_text(size = 15, face = "bold"))
  }


  of_int <- which((all_eval$method_id == "slingshot")&(all_eval$dataset_id == dta))
  if (!is.null(all_output[[of_int]]$models[[1]])){
    p_slingshot <- plot_dimred(all_output[[of_int]]$models[[1]], dimred = dta_tmp$dimred, color_cells = "grouping",
                               grouping = dynwrap::group_onto_nearest_milestones(dta_tmp)) +
      theme(legend.position = "none") +
      ggtitle(paste0("SLINGSHOT, mean_score = ", round(all_eval[of_int, "mean_score"], digits = 2)))+
      theme(plot.title = element_text(size = 15, face = "bold"))
    all_eval_slingshot <- all_eval[of_int,]
  } else {
    p_slingshot <- ggplot() + geom_point() +
      ggtitle("SLINGSHOT, mean_score = 0")+
      theme(plot.title = element_text(size = 15, face = "bold"))
  }

  of_int <- which((all_eval$method_id == "raceid_stemid")&(all_eval$dataset_id == dta))
  if (!is.null(all_output[[of_int]]$models[[1]])){
    dr <- dta_tmp$dimred[rownames(all_output[[of_int]]$models[[1]]$dimred),]
    p_raceid <- plot_dimred(all_output[[of_int]]$models[[1]], dimred = dr, color_cells = "grouping",
                            grouping = dynwrap::group_onto_nearest_milestones(dta_tmp)) +
      theme(legend.position = "none") +
      ggtitle(paste0("RACE ID, mean_score = ", round(all_eval[of_int, "mean_score"], digits = 2)))+
      theme(plot.title = element_text(size = 15, face = "bold"))
    all_eval_race_id <- all_eval[of_int,]
  } else {
    p_raceid <- ggplot() + geom_point() +
      ggtitle("RACE ID, mean_score = 0") +
      theme(plot.title = element_text(size = 15, face = "bold"))
  }

  of_int <- which((all_eval$method_id == "gng_param2")&(all_eval$dataset_id == dta))
  if (!is.null(all_output[[of_int]]$models[[1]])){
    p_gng <- plot_dimred(all_output[[of_int]]$models[[1]], dimred = dta_tmp$dimred, color_cells = "grouping",
                         grouping = dynwrap::group_onto_nearest_milestones(dta_tmp)) +
      theme(legend.position = "none") +
      ggtitle(paste0("TinGa, mean_score = ", round(all_eval[of_int, "mean_score"], digits = 2)))+
      theme(plot.title = element_text(size = 15, face = "bold"))
    all_eval_gng <- all_eval[of_int,]
  } else {
    p_gng <- ggplot() + geom_point() +
      ggtitle("TinGa, mean_score = 0")+
      theme(plot.title = element_text(size = 15, face = "bold"))
  }

  of_int <- which((all_eval$method_id == "monocle_3")&(all_eval$dataset_id == dta))
  if (!is.null(all_output[[of_int]]$models[[1]])){
    p_mon <- plot_dimred(all_output[[of_int]]$models[[1]], dimred = dta_tmp$dimred, color_cells = "grouping",
                         grouping = dynwrap::group_onto_nearest_milestones(dta_tmp)) +
      theme(legend.position = "none") +
      ggtitle(paste0("MONOCLE 3, mean_score = ", round(all_eval[of_int, "mean_score"], digits = 2)))+
      theme(plot.title = element_text(size = 15, face = "bold"))
    all_eval_monocle <- all_eval[of_int,]
  } else {
    p_mon <- ggplot() + geom_point() +
      ggtitle("Monocle_3, mean_score = 0")+
      theme(plot.title = element_text(size = 15, face = "bold"))
  }

  p <- list(gold_standard = p_gold,
            p_gng = p_gng,
            p_raceid = p_raceid,
            p_paga = p_paga,
            p_slingshot = p_slingshot,
            p_mon = p_mon)
  p
})

idx <- 1

grid.arrange(figures[[idx]]$gold_standard, figures[[idx]]$p_gng, figures[[idx]]$p_raceid,
             figures[[idx]]$p_paga, figures[[idx]]$p_slingshot, figures[[idx]]$p_mon, nrow = 2)

library(tidyverse)
library(workspace)
library(gng)
library(dyno)
library(ggplot2)
library(researchgng)
library(qsub)
library(RColorBrewer)
library(patchwork)

project("researchgng", "3.1_evaluate_gng_parameters_with_dyno")

load("results/eval_dynbenchmark/datasets_split.RData")
datasets_train <- split_datasets$datasets_train
load("derived_data/dataset_info.RData")

train_info <- dataset_info[which(dataset_info$id %in% datasets_train),]
test_info <- dataset_info[which(dataset_info$id %in% split_datasets$datasets_test),]

# epsilon_b <- lst("epsilon_b" = c(0.05, 0.1, 0.5))
# epsilon_n <- lst("epsilon_n" = c(0.001, 0.01, 0.1))
# age_max <- lst("age_max" = c(200, 100, 20))
# max_nodes <- lst("max_nodes" = c(15, 30, 50))
max_nodes <- lst("max_nodes" = c(seq(4, 20, by = 2), 30))
# lambda <- lst("lambda" = c(200, 100, 20))
# alpha <- lst("alpha" = c(0.2, 0.5, 0.8))
# beta <- lst("beta" = c(0.5, 0.9, 0.99))

default_params <- list("epsilon_b" = 0.05, "epsilon_n" = 0.001, "age_max" = 200,
                       "max_nodes" = 30, "lambda" = 200, "alpha" = 0.5, "beta" = 0.99)
# params_to_tweak <- c(epsilon_b, epsilon_n, age_max, max_nodes, lambda, alpha, beta)
params_to_tweak <- c(max_nodes)

combi <- generate_parameter_combinations(default_parameters = default_params,
                                         parameters_to_tweak = params_to_tweak)

combi2 <- crossing(
  combi,
  dataset = datasets_train
)

handle2 <- qsub_lapply(
  X = seq_len(nrow(combi2)),
  qsub_environment = c("combi2"),
  qsub_packages = c("tidyverse","workspace","gng","dyno","ggplot2","dplyr",
                    "dynbenchmark","tidyr","purrr","dynwrap","dynutils",
                    "dyndimred","stats","igraph","dyneval"),
  qsub_config = override_qsub_config(
    memory = "10G",
    max_wall_time = "24:00:00",
    name = "researchgng",
    wait = FALSE,
    remove_tmp_folder = FALSE,
    stop_on_error = FALSE
  ),

  FUN = function(i) {
    params <- combi2 %>% dynutils::extract_row_to_list(i)
    dataset <- dynbenchmark::load_dataset(params$dataset)
    eval <-
      dyneval::evaluate_ti_method(
        dataset = dataset,
        method = researchgng::gng_param2(),
        parameters = params[!names(params) %in% c("dataset")],
        metrics = c("correlation", "him", "featureimp_wcor", "F1_branches"),
        seed = 42
      )
    metrics <- eval$summary
    mean_score <- dynutils::calculate_geometric_mean(metrics$correlation,metrics$him,
                                                    metrics$featureimp_wcor,
                                                    metrics$F1_branches)
    names(mean_score) <- "geom_mean_score"
    eval$summary <- cbind(metrics, params, mean_score)
    #return(lst(eval, params))
    return(eval)
  })

saveRDS(handle2, "results/handle2.rds")
handle2 <- readRDS("results/handle2.rds")
output2<-qsub_retrieve(handle2)#, wait = "just_do_it")
save(output2, file="results/eval_gng_max_nodes_100_datasets.RData")
load("results/eval_gng_max_nodes_100_datasets.RData")

ix <- map_lgl(output2, ~length(.) != 1 && !is.na(.)) %>% which()
crashed <- map_lgl(output2, ~length(.) != 1 && !is.na(.)) %>% `!` %>% which()
not_crashed <- output2[ix]

eval <- output2[ix] %>% map_df(~.$summary) %>% mutate(ix = ix)
crashed <- output2 %>% keep(function(x) length(x) == 1)# %>% map_df(~.$summary)
eval_sum <- eval %>% select(
  ix,
  method_name,
  method_id,
  dataset_id,
  time_preprocessing,
  time_method,
  time_postprocessing,
  correlation,
  him,
  featureimp_wcor,
  F1_branches,
  one_of(names(default_params)),
  mean_score
)


## assess which parameter settings lead to higher mean ranking:
# param_sets <- unique(eval_sum[,names(default_params)]) %>%
#   mutate(param_setting = c("orig", "Eb_0.1", "Eb_0.5", "En_0.01", "En_0.1", "age_max_100",
#                    "age_max_20", "max_nodes_15", "max_nodes_50", "lambda_100",
#                    "lambda_20", "alpha_0.2", "alpha_0.8", "beta_0.5", "beta_0.9"))

# create a param_set vector, with names for the parameter settings in each of eval's rows
# eval_b <-
#   eval_sum %>%
#   left_join(param_sets, by = names(default_params))

# See scores:
ggplot(eval_sum, aes(x = max_nodes, y = mean_score, color = as.factor(max_nodes))) +
  geom_boxplot() +
  geom_jitter()

ggplot(eval_sum, aes(x = max_nodes, y = time_method, color = as.factor(max_nodes))) +
  geom_boxplot() +
  geom_jitter()

evalm <- reshape2::melt(eval_sum, id = "max_nodes",
                        measure = c("mean_score", "correlation", "F1_branches", "featureimp_wcor", "him", "time_method"))
ggplot(evalm, aes(as.factor(max_nodes), value, colour = as.factor(max_nodes))) +
  geom_boxplot() +
  #geom_jitter() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  facet_wrap(~variable, scales = "free")

evalb <- reshape2::melt(eval_sum, id = "max_nodes",
                        measure = c("mean_score", "time_method"))
ggplot(evalb, aes(as.factor(max_nodes), value, colour = as.factor(max_nodes))) +
  geom_boxplot() +
  #geom_jitter() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  facet_wrap(~variable, scales = "free") +
  theme(axis.title.y= element_blank(),
        #axis.title.x=element_blank(),
        text = element_text(size=18)) +
  xlab("Maximum number of units") +
  theme(legend.position = "none") +
  theme(axis.text.x = element_text(angle = 30, hjust = 1)) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))


# See it in heatmaps :

plot_heatmap_scores <- function(scores_dataframe, specific_datasets){
  scores_spe <- scores_dataframe %>%
    filter(source == specific_datasets) %>%
    group_by(trajectory_type) %>%
    summarize_if(is.numeric, mean)
  toplot <- as.matrix(scores_spe[,2:ncol(scores_spe)]) %>%
    magrittr::set_rownames(scores_spe$trajectory_type)
  sum_all <- apply(toplot, 2, mean)
  toplot2 <- as.data.frame(rbind(toplot, sum_all))
  pheatmap::pheatmap(toplot2, color = colorRampPalette(brewer.pal(11, "RdYlBu"))(100),
                     cluster_rows = F,
                     gaps_row=nrow(toplot))
}

plot_heatmap_scores(scores_df, "real/gold")

## assess which parameter settings lead to higher mean score:
tmp <-
  eval_b %>%
  left_join(dataset_info, by = c(dataset_id = "id")) %>%
  as_tibble() %>%
  group_by(trajectory_type, source, param_setting) %>%
  summarise(mean_score = mean(mean_score)) %>%
  ungroup()

tmp2 <- tmp %>%
  filter(source == "synthetic/dyngen")

bind_rows(
  tmp2,
  tmp2 %>%
    group_by(param_setting) %>%
    summarise_if(is.numeric, mean) %>%
    mutate(trajectory_type = "mean")
) %>%
  reshape2::acast(trajectory_type ~ param_setting, value.var = "mean_score") %>%
  pheatmap::pheatmap(cluster_cols = FALSE)


# Which gng 8 nodes were much better than gng 30 nodes?
eval_gng8 <- eval_sum %>%
  filter(max_nodes == 8)

eval_gng30 <- eval_sum %>%
  filter(max_nodes == 30)

# remove two datasets that crashed in gng30
eval_gng8 <- eval_gng8[-which(!eval_gng8$dataset_id %in% eval_gng30$dataset_id),]

plot(eval_gng8$mean_score,
     eval_gng30$mean_score,
     pch = 19,
     xlab = "Mean score GNG 8 nodes",
     ylab = "Mean score GNG 30 nodes")
abline(a = 0, b = 1)

dta_sel <- eval_gng8$dataset_id[which(eval_gng8$mean_score>0.9 & eval_gng30$mean_score<0.5)]

pdf("results/eval_dynbenchmark/different_max_nodes/gng8_gng30.pdf",
    width = 12,
    height = 8)
for (dta in dta_sel){
  print(paste0("Printing dataset ", dta))
  gs <- dynbenchmark::load_dataset(dta)
  dimred <- dyndimred::dimred(gs$expression(), method = "landmark_mds", ndim = 2)
  g0 <- plot_dimred(gs, dimred = dimred) +
    ggtitle(paste0("Gold standard on ", dta))
  iter_gng30 <- which(eval_sum$dataset_id == dta & eval_sum$max_nodes == 30)
  g1 <- plot_dimred(not_crashed[[iter_gng30]]$models[[1]]) +
    ggtitle(paste0("GNG with 30 nodes"))
  iter_gng8 <- which(eval_sum$dataset_id == dta & eval_sum$max_nodes == 8)
  g2 <- plot_dimred(not_crashed[[iter_gng8]]$models[[1]]) +
    ggtitle(paste0("GNG with 8 nodes"))

  # scores:
  scores_gng30 <- eval_sum[iter_gng30,c("mean_score", "correlation", "him", "F1_branches",
                        "featureimp_wcor")]
  scores_gng8 <- eval_sum[iter_gng8,c("mean_score", "correlation", "him", "F1_branches",
                                        "featureimp_wcor")]
  both <- bind_rows(
    scores_gng30 %>% mutate(method = "GNG30"),
    scores_gng8 %>% mutate(method = "GNG8")
  ) %>% gather(metric, score, -method)
  g3 <- ggplot(both, aes(method, score, colour = metric, group = metric)) +
    geom_point()+
    geom_line()

  gridExtra::grid.arrange(g0, g3, g1, g2, ncol = 2)
}
dev.off()

# plot gng :
# one of the worst : iter = 261
# best:
iter = which.max(eval_sum$mean_score)
eval_sum[iter,]

output_ix <- eval_sum$ix[iter]
traj <- output2[[output_ix]]$models[[1]]
gng2plot <- traj$gng_out
dimred <- traj$dimred

dataset <- dynbenchmark::load_dataset(paste0(eval_sum[iter, "dataset_id"]))
exprs <- dynwrap::get_expression(dataset)

plot_graph(dataset)
plot_dimred(dataset)
plot_dimred(traj)
plot_heatmap(dataset, features_oi = 100)

gngplot <- gng_project(gng2plot,
                        x = dimred, make_projection = T)
#gngplot <- gng_project(gng2plot, make_projection = F)

nodes_df <- data.frame(
  gng2plot$nodes,
  gngplot$node_proj
)

edges_df <- gng2plot$edges %>%
  left_join(nodes_df %>% select(i = name, i_GNG_X = GNG_X, i_GNG_Y = GNG_Y), by = "i") %>%
  left_join(nodes_df %>% select(j = name, j_GNG_X = GNG_X, j_GNG_Y = GNG_Y), by = "j")

cells_df <- data.frame(
  cell_id = rownames(dimred),
  gngplot$space_proj
)

# rescale cells:
cells_df_rescaled <- cells_df %>%
  mutate(GNG_X_rescaled = scales::rescale(GNG_X, to = c(min(nodes_df$GNG_X),
                                                        max(nodes_df$GNG_X))),
         GNG_Y_rescaled = scales::rescale(GNG_Y, to = c(min(nodes_df$GNG_Y),
                                                        max(nodes_df$GNG_Y))))

ggplot() +
  geom_segment(aes(x = i_GNG_X, xend = j_GNG_X, y = i_GNG_Y, yend = j_GNG_Y), edges_df, colour = "blue") +
  geom_point(aes(GNG_X_rescaled, GNG_Y_rescaled), cells_df_rescaled, alpha = .5) +
  geom_point(aes(GNG_X, GNG_Y), nodes_df, colour = "blue") +
  theme_bw()


plot(gngplot$igraph)

bla <- edge_age
bla[which(edge_age<50)] <- "#FF000080"
bla[which((edge_age>50)&(edge_age<100))] <- "#FFFF0080"
bla[which((edge_age>100)&(edge_age<150))] <- "#00FF0080"
bla[which(edge_age>150)] <- "#00FFFF80"

plot(gngplot$igraph, layout = gngplot$node_proj,
     vertex.size = 5, vertex.color = "#00808080",
     vertex.label = NA, edge.color = bla)






## select a few datasets to run optimization of the parameters on :
eval_o <- eval_b %>%
  left_join(dataset_info, by = c(dataset_id = "id")) %>%
  filter(param_setting=="orig") %>%
  mutate(rowname = dataset_id) %>%
  select(c("rowname", "time_method", "correlation", "him", "featureimp_wcor",
           "F1_branches", "mean_score", "source", "trajectory_type")) %>%
  column_to_rownames()

mds <- dyndimred::dimred_landmark_mds(eval_o[,1:6], ndim=2)
toplot <- merge.data.frame(as.data.frame(mds), eval_o, by = "row.names")

ggplot2::ggplot(toplot, aes(comp_1, comp_2, colour = source)) + geom_point()

library(dendextend)

dist_eval <- dist(as.matrix(eval_o[,!names(eval_o) %in% c("source", "trajectory_type")]))
clust_eval <- hclust(dist_eval, method = "ward.D2")
dend <- as.dendrogram(clust_eval)
labels_colors(dend) <- rainbow(6)[sort_levels_values(
  as.numeric(as.factor(eval_o$trajectory_type))[order.dendrogram(dend)]
)]
dend <- set(dend, "labels_cex", 0.7)

plot(dend,
     main = "Clustering of the datasets",
     horiz =  F,  nodePar = list(cex = .007))
legend("topleft", legend = c("non_tolerant","primary_tol","secondary_tol"), fill = c("red","green","blue"),cex=0.75)

clusts <- cutree(clust_eval, k = 20)
table(clusts)

eval_clust_info <- eval_o %>%
  mutate(clustering = clusts) %>%
  group_by(clustering) %>%
  summarize_if(is.numeric, c(mean, sd))

## select 1 dataset per cluster of > 10 datasets
selected_files <- eval_o %>%
  rownames_to_column() %>%
  mutate(clustering = clusts) %>%
  filter(clustering %in% which(table(clusts)>10)) #%>%
  # group_by(clustering) %>%
  # summarise(sample = sample(rowname, size = 1)) %>%
  # select(sample)

table(selected_files$clustering)


## isolate the most representative dataset of each cluster?

datasets_select <- lapply(names(table(selected_files$clustering)), function(file_idx){
  cluster_med <- selected_files %>%
    filter(clustering == file_idx) %>%
    summarize_if(is.numeric, median)

  cluster <- selected_files %>%
    filter(clustering == file_idx) %>%
    column_to_rownames() %>%
    select_if(is.numeric)

  correlations <- apply(cluster, 1, function(row){
    cor_row <- cor(as.numeric(row), as.numeric(cluster_med))
  })
  sel_file <- rownames(cluster)[which(correlations == max(correlations))]
  sel_file
})

visualise_datasets(unlist(datasets_select), directory = "results/dataset_plots/")
save(datasets_select, file = "results/optimization/selected_datasets.RData")




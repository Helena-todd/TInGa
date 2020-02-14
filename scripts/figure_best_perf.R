library(dplyr)
library(ggplot2)

# Identify, for each dataset, which method performed best, and make a figure
# out of it

load("results/eval_dynbenchmark/for_paper_eval_and_output3.RData")
all_eval <- list_results$all_eval

load("results/eval_dynbenchmark/datasets_split.RData")
datasets_test <- split_datasets$datasets_test
datasets <- datasets_test

all_eval <- all_eval %>%
  filter(dataset_id %in% datasets_test)
all_eval <- all_eval[which(all_eval$method_id %in% c("gng_param2", "slingshot", "monocle_3", "paga",
                                                     "raceid_stemid")),]
# datasets <- dynbenchmark::list_datasets()$id
load("derived_data/dataset_info_with_nb_cells.RData")


best_method <- lapply(seq_along(datasets), function(i){
  dta <- datasets[i]
  all_tp <- all_eval %>% filter(dataset_id == dta)
  all_tp$method_id[which.max(all_tp$mean_score)]
})

dta_per_method <- table(unlist(best_method))
names(dta_per_method)[1] <- "tinga"


best_p <- lapply(seq_along(datasets), function(i){
  dta <- datasets[i]
  all_tp <- all_eval %>% filter(dataset_id == dta)
  all_tp$method_id[which.max(all_tp$mean_score)]
}) %>% unlist %>% as.data.frame() %>%
  magrittr::set_colnames("method")

best_p$method <- as.character(best_p$method)
best_p$method[which(best_p$method == "gng_param2")] <- "tinga"

data_tmp <- dataset_info %>%
  column_to_rownames("id")
data_tmp <- data_tmp[datasets,]
best_p$trajectory_type <- data_tmp$trajectory_type

best_p$method <- factor(best_p$method,
                        levels = c("tinga", "monocle_3", "slingshot", "paga",
                                   "raceid_stemid"))

best_p$trajectory_type <- factor(best_p$trajectory_type,
                                 levels = c("linear", "bifurcation", "convergence", "cycle",
                                            "multifurcation", "tree", "acyclic_graph",
                                            "graph", "disconnected_graph"))

ggplot(best_p) +
  geom_bar(mapping = aes(x = method, fill = method, y = ..prop.., group = 1))

ggplot(best_p) +
  geom_bar(mapping = aes(x = method, fill = method))

count_fake = count(best_p, method, trajectory_type)
# fill in the missing combination
count_fake = complete(count_fake, method, trajectory_type)
# plot the data
ggplot(count_fake, aes(x = trajectory_type, y = n, fill = method)) +
  geom_bar(stat = "identity", position = "dodge") +
  theme(legend.text=element_text(size=16)) +
  theme(axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        text = element_text(size=18)) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))

ggplot(count_fake, aes(x = method, y = n, fill = trajectory_type)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_brewer(drop=FALSE, palette = "YlOrRd") +
  scale_y_continuous(limits=c(0,21)) +
  scale_x_discrete(drop=FALSE) +
  theme(legend.text=element_text(size=16)) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  theme(axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        text = element_text(size=18)) +
  geom_segment(x = 0.6, y = 19.5, xend = 1.5, yend = 19.5) +
  annotate(geom="text", x=1.05, y=20.5,
           label=as.character(dta_per_method[names(table(best_p$method))[1]]),
           size = 6) +
  geom_segment(x = 1.6, y = 19, xend = 2.5, yend = 19) +
  annotate(geom="text", x=2.05, y=20,
           label=as.character(dta_per_method[names(table(best_p$method))[2]]),
           size = 6) +
  geom_segment(x = 2.6, y = 18.5, xend = 3.5, yend = 18.5) +
  annotate(geom="text", x=3.05, y=19.5,
           label=as.character(dta_per_method[names(table(best_p$method))[3]]),
           size = 6) +
  geom_segment(x = 3.6, y = 15.5, xend = 4.5, yend = 15.5) +
  annotate(geom="text", x=4.05, y=16.5,
           label=as.character(dta_per_method[names(table(best_p$method))[4]]),
           size = 6) +
  geom_segment(x = 4.6, y = 10, xend = 5.5, yend = 10) +
  annotate(geom="text", x=5, y=11,
           label=as.character(dta_per_method[names(table(best_p$method))[5]]),
           size = 6)



# count the data
count_fake = count(best_p, method, trajectory_type)
# fill in the missing combination
count_fake = complete(count_fake, method, trajectory_type)
# plot the data
ggplot(count_fake, aes(x = trajectory_type, y = n, fill = method)) +
  geom_bar(stat = "identity", position = "dodge") +
  theme(legend.text=element_text(size=16)) +
  theme(axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        text = element_text(size=18)) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))



ggplot(count_fake, aes(x = method, y = n, fill = trajectory_type)) +
  geom_bar(stat = "identity", position="stack") +
  scale_fill_brewer(drop=FALSE, palette = "YlOrRd") +
  theme(legend.text=element_text(size=20)) +
  theme(axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        text = element_text(size=20),
        axis.text.x = element_text(angle = 45, hjust = 1)) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))

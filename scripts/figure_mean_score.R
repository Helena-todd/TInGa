library(ggplot2)
library(dplyr)

# Mean scores of the different methods

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

# p1 <- ggplot(all_eval, aes(x = method_id, y = mean_score, color = method_id)) +
#   geom_boxplot(outlier.size = 0.1) +
#   # scale_y_log10()+
#   geom_jitter(outlier.size = 0.1) +
#   theme(axis.title.x=element_blank(),
#         axis.title.y=element_blank(),
#         text = element_text(size=18)) +
#   theme(axis.text.x = element_text(angle = 30, hjust = 1))
#
# p1
colnames(all_eval) <- c("method_id", "dataset_id", "e)  Mean Score",
                        "d)  HIM", "b)  F1 Branches",
                        "a)  Correlation", "c)  FeatureImp wCor",
                        "f)  Time Method")
# evalr <- reshape2::melt(all_eval, id = "method_id",
#                         measure = c("correlation", "F1_branches", "featureimp_wcor","him", "mean_score","time_method"))

evalr <- reshape2::melt(all_eval, id = "method_id",
                        measure = c("a)  Correlation", "b)  F1 Branches",
                                    "c)  FeatureImp wCor","d)  HIM",
                                    "e)  Mean Score","f)  Time Method"))


nb_cells <- dataset_info[,c("id", "nb_cells")] %>%
  column_to_rownames("id")
nb_cells2 <- nb_cells[all_eval$dataset_id,]
class_nb_cells <- nb_cells2
class_nb_cells[which(nb_cells2 < 500)] <- "< 500"
class_nb_cells[which((nb_cells2 >= 500)&(nb_cells2 < 2000))] <- "500-2000"
class_nb_cells[which((nb_cells2 >= 2000)&(nb_cells2 < 5000))] <- "2000-5000"
class_nb_cells[which((nb_cells2 >= 5000)&(nb_cells2 < 10000))] <- "5000-10000"
class_nb_cells[which(nb_cells2 >= 10000)] <- "> 10000"


# time_info3 <- all_eval %>%
#   mutate(nb_cells = nb_cells2,
#          class_nb_cells = class_nb_cells) %>%
#   group_by(method_id, class_nb_cells) %>%
#   summarise_all(funs(mean(., na.rm = TRUE)))

time_info3b <- matrix(c(11, 11, 13, 16, 21,
                        8, 10, 16, 31, 43,
                        10, 18, 147, 1625, 5102,
                        6, 10, 32, 111, 211,
                        47, 172, 269, "-", "-"), nrow = 5)
                        #8, 14, 31, 129, "NaN" ), nrow = 5)
rownames(time_info3b) <- c("< 500", "500-2k", "2k-5k", "5k-10k", "> 10k")
colnames(time_info3b) <- c("TinGa", "Monocle3", "Slingshot", "PAGA", "StemID")
tt <- ttheme_minimal()
tbl <- tableGrob(time_info3b, rows=rownames(time_info3b), theme=tt)


evalr_tmp <- evalr %>%
  filter(!variable %in% c("f)  Time Method", "e)  Mean Score"))
evalr_tmp$method_id <- factor(evalr_tmp$method_id,
                          levels = c("tinga", "monocle_3", "slingshot", "paga",
                                     "raceid_stemid"))

facet_plot <-ggplot(evalr_tmp, aes(method_id, value, colour = method_id)) +
  geom_boxplot(outlier.size = 0.1) +
  # geom_jitter() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  facet_wrap(~variable, scales = "free", nrow = 3) +
  theme(axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        text = element_text(size=18)) +

  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))+
  theme(legend.position = "none")

# theme(legend.position = c(1, 0), legend.justification = c(1, 0))

evalr_tmp2 <- evalr %>%
  filter(variable == c("e)  Mean Score"))
evalr_tmp2$method_id <- factor(evalr_tmp2$method_id,
                              levels = c("tinga", "monocle_3", "slingshot", "paga",
                                         "raceid_stemid"))

facet_plot2 <-ggplot(evalr_tmp2, aes(method_id, value, colour = method_id)) +
  geom_boxplot(outlier.size = 0.1) +
  # geom_jitter() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  facet_wrap(~variable, scales = "free", nrow = 3) +
  theme(axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        text = element_text(size=18)) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))+
  theme(legend.position = "none")

lay <- rbind(c(1,1),
             c(1,1),
             c(2,3))

dd1 <- ggplot() +
  annotation_custom(tableGrob(time_info3b, rows=rownames(time_info3b), theme=tt2)) +
  ggtitle('f) Running time of the methods') +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "white"))

grid.arrange(grobs = list(facet_plot,
                          facet_plot2,
                          dd1),
             layout_matrix = lay)







# time_tmp <- time_info2
# colnames(time_tmp)[8] <- "f)  Time Method"
# timer <- reshape2::melt(time_tmp, id = "method_id",
#                         measure = c("f)  Time Method"))
# timer$value <- round(timer$value)
#
# time_plot <-ggplot(timer, aes(x = method_id, y = value, fill = method_id)) +
#   geom_bar(stat="identity") +
#   geom_text(aes(label=value), vjust=0, size = 5) +
#   scale_y_continuous(limits=c(0,300)) +
#   # geom_jitter() +
#   facet_wrap(~variable) +
#   theme(axis.title.x=element_blank(),
#         axis.title.y=element_blank(),
#         text = element_text(size=18),
#         axis.text.x = element_text(angle = 45, hjust = 1)) +
#   theme(legend.position = "none")


# vp <- grid::viewport(width = 0.34, height = 0.517, x = 0.83, y = 0.25)
# print(facet_plot)
# print(time_plot, vp = vp)

# plot(tbl, vp = vp)


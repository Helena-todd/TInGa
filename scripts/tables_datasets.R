load("derived_data/dataset_info_with_nb_cells.RData")
load("results/eval_dynbenchmark/datasets_split.RData")

info_test <- dataset_info[which(dataset_info$id %in% split_datasets$datasets_test),]
info_test_real <- info_test %>% filter(grepl("real", info_test$id))
table(info_test_real$trajectory_type)

info_test_synth <- info_test %>% filter(grepl("synthetic", info_test$id))
table(info_test_synth$trajectory_type)


info_train <- dataset_info[which(dataset_info$id %in% split_datasets$datasets_train),]
info_train_real <- info_train %>% filter(grepl("real", info_train$id))
table(info_train_real$trajectory_type)

info_train_synth <- info_train %>% filter(grepl("synthetic", info_train$id))
table(info_train_synth$trajectory_type)

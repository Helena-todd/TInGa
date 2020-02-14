library(dynbenchmark)
library(tidyverse)
library(furrr)
plan(multiprocess)

load("results/eval_dynbenchmark/for_paper_eval_and_output3.RData")
load("derived_data/dataset_info_with_nb_cells.RData")

load("results/eval_dynbenchmark/datasets_split.RData")
datasets_test <- split_datasets$datasets_test

all_eval <- list_results$all_eval
idx <- which(all_eval$dataset_id %in% datasets_test)
output2 <- list_results$all_output[idx]

ix <- map_lgl(output2, ~length(.) != 1 && !is.na(.)) %>% which()
crashed <- map_lgl(output2, ~length(.) != 1 && !is.na(.)) %>% `!` %>% which()
not_crashed <- output2[ix]

eval <- output2[ix] %>% map_df(~.$summary) %>% mutate(ix = ix)
eval$method_name[which(eval$method_name == "GNG param2")] <- "TinGa"
eval$method_name[which(eval$method_name == "MONOCLE3 param")] <- "MONOCLE3"
eval$method_id[which(eval$method_id == "gng_param2")] <- "tinga"
eval$method_id[which(eval$method_id == "monocle3_param")] <- "monocle3"

## only take into account the 250 test datasets
load("results/eval_dynbenchmark/datasets_split.RData")
datasets_test <- split_datasets$datasets_test

to_keep <- which(eval$dataset_id %in% datasets_test)

# # only take into account methods with free topology and tree detection
# relevant_methods <- c("gng_param", "paga_tree", "raceid_stemid")

#output2 <- output2[ix]
output2 <- output2[to_keep]

output <- data.frame(method_id = lapply(output2, function(x){
  x$summary$method_id
}) %>% unlist(),
                      dataset_id = lapply(output2, function(x){
                        x$summary$dataset_id
                      }) %>% unlist(),
                      him = lapply(output2, function(x){
                        x$summary$him
                      }) %>% unlist()) %>%
  select(method_id, dataset_id, him) #%>%
  #filter(method_id %in% relevant_methods)

# simplify all milestone networks
simplify_milestone_network <- function(milestone_network) {
  milestone_network %>%
    igraph::graph_from_data_frame(directed = first(milestone_network$directed)) %>%
    dynwrap::simplify_igraph_network() %>%
    igraph::as_data_frame()
}

output$milestone_network <- lapply(seq_along(output2), function(idx){
  print(idx)
  x <- output2[[idx]]
  if(is.null(x$models[[1]]$milestone_network)){
    NA
  } else {
    x$models %>%
      map("milestone_network") %>%
      future_map(simplify_milestone_network)
  }
})


# calculate milestone network statistics
calculate_milestone_network_statistics <- function(milestone_network) {
  lst(
    n_nodes = length(unique(c(milestone_network$from, milestone_network$to))),
    n_edges = nrow(milestone_network),
    complexity = n_nodes + n_edges
  )
}

output <- output %>% filter(!is.na(milestone_network))

milestone_network_stats <- map_df(output$milestone_network, function(x){
  calculate_milestone_network_statistics(x[[1]])
})


prediction_statistics <- bind_cols(
  output %>% select(-milestone_network),
  milestone_network_stats
)

# do the same for the datasets
output$dataset_id <- as.character(output$dataset_id)
datasets <- load_datasets(ids = unique(output$dataset_id))

datasets$milestone_network <- datasets$milestone_network %>%
  future_map(simplify_milestone_network)

dataset_statistics <- future_map_dfr(datasets$milestone_network, calculate_milestone_network_statistics) %>%
  mutate(dataset_id = datasets$id)
dataset_statistics$trajectory_type_dataset <- datasets$trajectory_type[match(dataset_statistics$dataset_id, datasets$id)]

# now combine and compare
statistics <- left_join(
  prediction_statistics,
  dataset_statistics,
  c("dataset_id"),
  suffix = c("_prediction", "_dataset")
)

# calculate some difference statistics and add information on the methods
statistics <- statistics %>%
  mutate(
    complexity_difference = complexity_prediction - complexity_dataset
  )

# add him
statistics$him <- output$him

# add means
statistics <- statistics %>%
  group_by(method_id) %>%
  mutate_if(is.numeric, funs(mean = mean)) %>%
  ungroup()

# and save the statistics
write_rds(statistics, "~/Desktop/statistics_for_paper3.rds")

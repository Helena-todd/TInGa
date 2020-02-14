library(tidyverse)
library(workspace)
library(gng)
library(dyno)
library(researchgng)

project("researchgng", "1_evaluate_gng_with_dyno")

## !! needed to install :
## - netdist from github dynverse/netdist
## - GO.db, impute, minet from bioconductor
## in order to use function "dyneval::evaluate_ti_method()"


dataset <- dynbenchmark::load_dataset("real/gold/aging-hsc-old_kowalczyk")

eval <- dyneval::evaluate_ti_method(
  dataset = dataset,
  method = gng_param(),
  parameters = list(),
  metrics = c("correlation", "him", "featureimp_wcor", "F1_branches")
)

eval$summary

model <- eval$models[[1]]
plot_graph(model)
plot_dimred(model)

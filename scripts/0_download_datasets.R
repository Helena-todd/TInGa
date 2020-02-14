#' Downloading the processed datasets from Zenodo ([10.5281/zenodo.1443566](https://doi.org/10.5281/zenodo.1443566))

library(tidyverse)
library(workspace)
library(httr)

project("researchgng", "0_datasets")

# -> clone dynbenchmark and run scripts/01-datasets/01-download_from_zenodo.R in order to get the datasets

library(dplyr)
library(dynwrap)
library(dynplot)
library(tidyverse)
library(workspace)
library(gng)
library(dyno)
library(ggplot2)
library(researchgng)

project("researchgng", "3.4_evaluate_gng_top_ti_methods_with_dyno")

seur <- readRDS("~/Downloads/seu_dataset.rds")
Seurat::DimPlot(seur)

# pancreas <- read.csv("~/count_menfiche.csv", sep = " ", header = F)
# pancreas_cell_info <- read.csv("~/cells_info_menfiche.csv", sep = ",")
# pancreas_gene_info <- read.csv("~/vars_info_menfiche.csv", sep = ",")
#
# colnames(pancreas) <- pancreas_gene_info$index
# rownames(pancreas) <- pancreas_cell_info$index
# pancreas <- as.matrix(pancreas)

dim(seur$orig.ident)

normalised_seur <- seur@assays$spliced@scale.data
pca70_seur <- seur@reductions$pca@cell.embeddings
expression <- pca70_seur

dta <- wrap_expression(expression = expression, counts = expression)
gng_res <- dynwrap::infer_trajectories(dataset = dta,
                                       method = list(
                                         "gng_param2" = researchgng::gng_param2(max_nodes = 8, ndim = 10)
                                       ))

p_gng <- plot_dimred(gng_res$model[[1]]) + ggtitle("GNG")

slg_res <- dynwrap::infer_trajectories(dataset = dta,
                                       method = list(
                                         slingshot = ti_slingshot()
                                       ))

p_slg <- plot_dimred(slg_res$model[[1]])+ ggtitle("Slingshot")

pag_res <- dynwrap::infer_trajectories(dataset = dta,
                                       method = list(
                                         paga_tree = ti_paga_tree()
                                       ))

p_pag <- plot_dimred(pag_res$model[[1]])+ ggtitle("PAGA Tree")

mon_res <- dynwrap::infer_trajectories(dataset = dta,
                                       method = list(
                                         "monocle3" = researchgng::monocle3_param()
                                       ))

p_mon <- plot_dimred(mon_res$model[[1]])+ ggtitle("Monocle3")

rac_res <- dynwrap::infer_trajectories(dataset = dta,
                                       method = list(
                                         race_id = ti_raceid_stemid()
                                       ))

p_rac <- plot_dimred(rac_res$model[[1]])+ ggtitle("Race ID/ Stem ID")
rac_res$summary[[1]]$error

gridExtra::grid.arrange(p_gng, p_slg, p_mon)

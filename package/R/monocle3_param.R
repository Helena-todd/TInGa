#' @export
#' @importFrom dynwrap create_ti_method_r
#' @import dynparam
monocle3_param <- dynwrap::create_ti_method_r(
  definition = definition(
    method = def_method(
      id = "monocle3_param",
      name = "MONOCLE3 param"
    ),

    wrapper = def_wrapper(
      # describe run fun inputs and outputs
      input_required = "expression",
      input_optional = NULL
    ),

    # describe tuneable parameters
    parameters = parameter_set(
      integer_parameter(
        id = "ndim",
        default = 5L,
        distribution = uniform_distribution(2L, 10L),
        description = "The number of dimensions"
      )
    )
  ),

  # describe packages needed by method
  package_loaded = c("monocle3"),
  package_required = c("BiocGenerics", "DelayedArray", "DelayedMatrixStats",
                       "limma", "S4Vectors", "SingleCellExperiment",
                       "SummarizedExperiment", "batchelor"),

  run_fun = function(
    expression,
    parameters,
    seed = NULL,
    verbose = FALSE
  ) {
    # TIMING: done with preproc
    checkpoints <- list(method_afterpreproc = as.numeric(Sys.time()))

    # perform (mandatory) data preprocessing (normalisation + PCA)
    cds <- new_cell_data_set(expression_data = t(expression))
    cds <- preprocess_cds(cds, num_dim = parameters$ndim)

    # perform (mandatory) dimensionality reduction: UMAP
    cds <- reduce_dimension(cds)

    # perform (mandatory) clustering
    cds <- cluster_cells(cds)


    # calculate trajectory
    cds <- learn_graph(cds)

    # transform to milestone network
    node_dist2 <- stats::dist(t(cds@principal_graph_aux$UMAP[[7]])) %>% as.matrix
    table_edges <- unlist(igraph::get.edgelist(cds@principal_graph$UMAP))
    milestone_network2 <- table_edges %>%
      as.data.frame() %>%
      select(from = V1, to = V2) %>%
      mutate(
        length = node_dist2[cbind(from, to)],
        directed = FALSE,
        "from" = as.character(from),
        "to" = as.character(to)
      ) %>%
      select(from, to, length, directed)

    # compute dimred milestones & segments
    dimred_milestones2 <- t(cds@principal_graph_aux$UMAP[[7]])
    colnames(dimred_milestones2) <- c("comp_1", "comp_2")
    dimred_trajectory_segments2 <- cbind(
      dimred_milestones2[milestone_network2$from, , drop = FALSE] %>% magrittr::set_colnames(paste0("from_", colnames(dimred_milestones2))),
      dimred_milestones2[milestone_network2$to, , drop = FALSE] %>% magrittr::set_colnames(paste0("to_", colnames(dimred_milestones2)))
    )

    # TIMING: done with method
    checkpoints$method_aftermethod <- as.numeric(Sys.time())

    dr <- cds@int_colData$reducedDims$UMAP
    colnames(dr) <- c("comp_1", "comp_2")

    # return output
    traj <-
      wrap_data(
        cell_ids = rownames(expression)
      ) %>%
      add_dimred_projection(
        milestone_ids = rownames(dimred_milestones2),
        milestone_network = milestone_network2,
        dimred_milestones = dimred_milestones2,
        dimred = dr,
        dimred_trajectory_segments = dimred_trajectory_segments2
      ) %>%
      add_timings(
        timings = checkpoints
      )
  },
  return_function = TRUE
)

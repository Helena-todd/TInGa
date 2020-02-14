#' @export
#' @importFrom dynwrap create_ti_method_r
#' @import dynparam
gng_param <- dynwrap::create_ti_method_r(
  definition = definition(
    method = def_method(
      id = "gng_param",
      name = "GNG param"
    ),

    wrapper = def_wrapper(
      # describe run fun inputs and outputs
      input_required = "expression",
      input_optional = NULL
    ),

    # describe tuneable parameters
    parameters = parameter_set(
      character_parameter(
        id = "dimred_method",
        default = "landmark_mds",
        # values = names(dyndimred::list_dimred_methods()),
        values = "landmark_mds",
        description = "Which dimensionality reduction method to use"
      ),
      character_parameter(
        id = "gng_method",
        default = "researchgng::GNG",
        # values = names(dyndimred::list_dimred_methods()),
        values = c("researchgng::GNG", "researchgng::GNG_km_init"),
        description = "Which GNG method to use"
      ),
      integer_parameter(
        id = "ndim",
        default = 5L,
        distribution = uniform_distribution(2L, 10L),
        description = "The number of dimensions"
      ),
      integer_parameter(
        id = "max_iter",
        default = 20000L,
        distribution = expuniform_distribution(25L, 100000L),
        description = "The max number of iterations"
      ),
      integer_parameter(
        id = "max_nodes",
        default = 30L,
        distribution = uniform_distribution(2L, 50L),
        description = "The maximum number of nodes"
      ),
      numeric_parameter(
        id = "epsilon_b",
        default = 0.05,
        distribution = uniform_distribution(0,1),
        description = "Value of which the closest node will move"
      ),
      numeric_parameter(
        id = "epsilon_n",
        default = 0.001,
        distribution = expuniform_distribution(0.00001, 1),
        description = "Value of which the second closest nodes will move"
      ),
      integer_parameter(
        id = "age_max",
        default = 200,
        distribution = uniform_distribution(20, 1000),
        description = "Maximum age of an edge before it's removed"
      ),
      integer_parameter(
        id = "lambda",
        default = 200,
        distribution = uniform_distribution(20, 1000),
        description = "A new node can be added every lambda iteration"
      ),
      numeric_parameter(
        id = "alpha",
        default = 0.5,
        distribution = uniform_distribution(0, 1),
        description = "1-percentage of error attributed to closest node"
      ),
      numeric_parameter(
        id = "beta",
        default = 0.99,
        distribution = uniform_distribution(0, 1),
        description = "Reduction of error of all nodes by beta"
      ),
      logical_parameter(
        id = "apply_mst",
        default = FALSE,
        description = "If true, an MST post-processing of the GNG is performed."
      )
    )
  ),

  # describe packages needed by method
  package_loaded = c("dplyr", "tidyr", "purrr", "dynwrap", "dynutils"),
  package_required = c("dyndimred", "gng", "stats", "igraph"),

  run_fun = function(
    expression,
    parameters,
    seed = NULL,
    verbose = FALSE
  ) {
    # TIMING: done with preproc
    checkpoints <- list(method_afterpreproc = as.numeric(Sys.time()))

    # perform dimensionality reduction
    dimred <- dyndimred::dimred(expression, method = parameters$dimred_method, ndim = parameters$ndim)

    # calculate GNG
    if(parameters$gng_method == "researchgng::GNG"){
      gng_out <- researchgng::GNG(
        dimred,
        max_iter = parameters$max_iter,
        max_nodes = parameters$max_nodes,
        epsilon_b = parameters$epsilon_b,
        epsilon_n = parameters$epsilon_n,
        age_max = parameters$age_max,
        lambda = parameters$lambda,
        alpha = parameters$alpha,
        beta = parameters$beta
      )
    } else if (parameters$gng_method == "researchgng::GNG_km_init"){
      gng_out <- researchgng::GNG_km_init(
        dimred,
        max_iter = parameters$max_iter,
        max_nodes = parameters$max_nodes,
        epsilon_b = parameters$epsilon_b,
        epsilon_n = parameters$epsilon_n,
        age_max = parameters$age_max,
        lambda = parameters$lambda,
        alpha = parameters$alpha,
        beta = parameters$beta
      )
    }
    node_dist <- stats::dist(gng_out$node_space) %>% as.matrix

    # transform to milestone network
    node_names <- gng_out$nodes %>% mutate(name = as.character(node))
    milestone_network <- gng_out$edges %>%
      select(from = i, to = j) %>%
      mutate(
        length = node_dist[cbind(from, to)],
        directed = FALSE,
        "from" = as.character(from),
        "to" = as.character(to)
      ) %>%
      select(from, to, length, directed)

    # apply MST, if so desired
    if (parameters$apply_mst) {
      gr <- igraph::graph_from_data_frame(milestone_network, directed = F, vertices = node_names$name)
      milestone_network <- igraph::minimum.spanning.tree(gr, weights = igraph::E(gr)$length) %>% igraph::as_data_frame()
    }

    # compute dimred milestones & segments
    dimred_milestones <- gng_out$node_space
    rownames(dimred_milestones) <- c(1:nrow(dimred_milestones))
    dimred_trajectory_segments <- cbind(
      dimred_milestones[milestone_network$from, , drop = FALSE] %>% magrittr::set_colnames(paste0("from_", colnames(dimred_milestones))),
      dimred_milestones[milestone_network$to, , drop = FALSE] %>% magrittr::set_colnames(paste0("to_", colnames(dimred_milestones)))
    )

    # TIMING: done with method
    checkpoints$method_aftermethod <- as.numeric(Sys.time())

    # return output
    traj <-
      wrap_data(
        cell_ids = rownames(expression),
        gng_out = gng_out
      ) %>%
      add_dimred_projection(
        milestone_ids = rownames(dimred_milestones),
        milestone_network = milestone_network,
        dimred_milestones = dimred_milestones,
        dimred = dimred,
        dimred_trajectory_segments = dimred_trajectory_segments
      ) %>%
      add_timings(
        timings = checkpoints
      )
  },
  return_function = TRUE
)

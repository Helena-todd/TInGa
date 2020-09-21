#' @export
#' @importFrom dynwrap create_ti_method_r
#' @import dynparam
gng_param2 <- dynwrap::create_ti_method_r(
  definition = definition(
    method = def_method(
      id = "gng_param2",
      name = "GNG param2"
    ),

    wrapper = def_wrapper(
      # describe run fun inputs and outputs
      input_required = "expression",
      input_optional = "dimred"
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
        default = 8L,
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
      )
    )
  ),

  # describe packages needed by method
  package_loaded = c("dplyr", "tidyr", "purrr", "dynwrap", "dynutils"),
  package_required = c("dyndimred", "gng", "stats", "igraph"),

  run_fun = function(
    expression,
    parameters,
    priors = NULL,
    seed = NULL,
    verbose = FALSE
  ) {
    # TIMING: done with preproc
    checkpoints <- list(method_afterpreproc = as.numeric(Sys.time()))

    # perform dimensionality reduction
    # or reuse if prior is given
    if (is.null(priors$dimred)) {
      dimred <- dyndimred::dimred(expression, method = parameters$dimred_method, ndim = parameters$ndim)
    } else {
      dimred <- priors$dimred
    }

    # calculate GNG
    gng_out <- TInGa::GNG(
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

    node_dist <- stats::dist(gng_out$node_space) %>% as.matrix
    gng_out_orig <- gng_out

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

    # compute dimred milestones & segments
    dimred_milestones <- gng_out$node_space
    rownames(dimred_milestones) <- c(1:nrow(dimred_milestones))
    dimred_trajectory_segments <- cbind(
      dimred_milestones[milestone_network$from, , drop = FALSE] %>% magrittr::set_colnames(paste0("from_", colnames(dimred_milestones))),
      dimred_milestones[milestone_network$to, , drop = FALSE] %>% magrittr::set_colnames(paste0("to_", colnames(dimred_milestones)))
    )

    # TIMING: done with method
    # checkpoints$method_aftermethod <- as.numeric(Sys.time())

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
      ) # %>%
    # add_timings(
    #   timings = checkpoints
    # )

    traj_orig <- traj

    # perform MST
    milestone_network <- gng_out$edges %>%
      select(from = i, to = j) %>%
      mutate(
        length = node_dist[cbind(from, to)],
        directed = FALSE,
        "from" = as.character(from),
        "to" = as.character(to)
      ) %>%
      select(from, to, length, directed)

    gr <- igraph::graph_from_data_frame(milestone_network, directed = F, vertices = node_names$name)
    milestone_network <- igraph::minimum.spanning.tree(gr, weights = igraph::E(gr)$length) %>% igraph::as_data_frame()

    # compute dimred milestones & segments
    dimred_milestones <- gng_out$node_space
    rownames(dimred_milestones) <- c(1:nrow(dimred_milestones))
    dimred_trajectory_segments <- cbind(
      dimred_milestones[milestone_network$from, , drop = FALSE] %>% magrittr::set_colnames(paste0("from_", colnames(dimred_milestones))),
      dimred_milestones[milestone_network$to, , drop = FALSE] %>% magrittr::set_colnames(paste0("to_", colnames(dimred_milestones)))
    )

    # return output
    traj_mst <-
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
      )

    gng_out <- gng_out_orig
    traj <- traj_orig

    # post_processing, add edges if needed:
    milestone_network <- gng_out$edges %>%
      select(from = i, to = j) %>%
      mutate(
        length = node_dist[cbind(from, to)],
        directed = FALSE,
        "from" = as.character(from),
        "to" = as.character(to)
      ) %>%
      select(from, to, length, directed)

    gr <- igraph::graph_from_data_frame(milestone_network, directed = F, vertices = node_names$name)
    milestone_network2 <- igraph::minimum.spanning.tree(gr, weights = igraph::E(gr)$length) %>% igraph::as_data_frame()

    mean_densities <- lapply(seq_len(nrow(traj_mst$milestone_network)), function(edge_idx){
      edge <- as.character(traj_mst$milestone_network[edge_idx,c(1:2)])
      edge_cells <- traj_mst$progressions[which((traj_mst$progressions$from == min(edge))&
                                                  (traj_mst$progressions$to == max(edge))),]
      edge_length <- traj_mst$milestone_network[edge_idx, "length"]
      density_ratio <- nrow(edge_cells)/edge_length
    })
    mean_density <- mean(unlist(mean_densities))

    gr2 <- igraph::graph_from_data_frame(milestone_network2,
                                         directed = FALSE,
                                         vertices = gng_out$nodes)

    end_nodes <- as.character(which(igraph::degree(gr2) == 1))
    combis <- utils::combn(end_nodes, 2)

    for(edge in seq_len(ncol(combis))){
      edge_to_try <- combis[,edge]
      if(length(which((milestone_network$from == min(edge_to_try))&
                      (milestone_network$to == max(edge_to_try)))) != 0){

        edge_cells <- traj$progressions[which((traj$progressions$from == min(edge_to_try))&
                                                (traj$progressions$to == max(edge_to_try))),]
        edge_length <- traj$milestone_network[edge, "length"]
        density_ratio <- nrow(edge_cells)/edge_length

        if(density_ratio >= mean_density/2){
          milestone_network3 <- rbind(milestone_network2,
                                      milestone_network[which((milestone_network$from == min(edge_to_try))&
                                                                (milestone_network$to == max(edge_to_try))),])
          gr <- igraph::graph_from_data_frame(milestone_network3,
                                              directed = FALSE,
                                              vertices = gng_out$nodes)
          gr_triangles <- igraph::triangles(gr)
          if(length(gr_triangles) == 0){
            milestone_network2 <- milestone_network3
          }
        }
      }
    }

    node_names <- gng_out$nodes %>% mutate(name = as.character(node))
    milestone_network <- milestone_network2

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

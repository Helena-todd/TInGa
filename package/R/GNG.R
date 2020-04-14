#' GNG
#'
#' The Growing Neural Gas algorithm adapted to trajectory inference
#'
#' @param x the expression dataset
#' @param max_iter Maximum of iterations
#' @param The percentage by which the closest node will move
#' @param epsilon_n The percentage by which the neighbors of the closest node will move
#' @param age_max Maximum age of the edges
#' @param max_nodes Maximum of nodes in the graph
#' @param lambda iteration where new nodes can be inserted
#' @param alpha error of the closest node
#' @param beta percentage by which the errors of all nodes will decrease at every iteration
#' @param verbose boolean
#' @param make_logs_at At which iterations to store the GNG, for visualisation purposes.
#'
#' @return A gng object
#' @export
#'
GNG <- function (x, max_iter, epsilon_b, epsilon_n, age_max, max_nodes,
          lambda, alpha, beta, verbose = TRUE, make_logs_at = NULL)
{
  ranges <- apply(x, 2, range)
  S <- matrix(NA, nrow = max_nodes, ncol = ncol(x), dimnames = list(NULL,
                                                                    colnames(x)))
  S_meta <- data.frame(i = seq_len(nrow(S)), error = rep(0,
                                                         nrow(S)))
  E <- lapply(seq_len(max_nodes), function(i) numeric(0))
  E[[1]] <- 2
  E[[2]] <- 1
  Age <- matrix(NA, nrow = max_nodes, ncol = max_nodes)
  S[1, ] <- apply(ranges, 2, function(r) {
    stats::qunif(.25, r[[1]], r[[2]])
  })
  S[2, ] <- apply(ranges, 2, function(r) {
    stats::qunif(.75, r[[1]], r[[2]])
  })
  Age[1, 2] <- 0
  Age[2, 1] <- 0
  nextr <- 3
  distance_function <- function(xi, Si) {
    diff <- xi - Si
    sqrt(mean(diff * diff))
  }
  move_function <- function(xi, Si, epsilon) {
    Si + epsilon * (xi - Si)
  }
  sample_input_signal <- function() {
    x[sample.int(nrow(x), 1), ]
  }
  current.iter <- 0L
  log <- list()
  if (current.iter %in% make_logs_at) {
    current_log <- list(current.iter = current.iter, S = S,
                        S_meta = S_meta, Age = Age, E = E)
    log[[length(log) + 1]] <- current_log
  }
  while (current.iter <= max_iter) {
    current.iter <- current.iter + 1L
    if (verbose && current.iter%%1000L == 0L)
      cat("Iteration ", current.iter, "\n", sep = "")
    xi <- sample_input_signal()
    sdist <- apply(S, 1, function(Si) distance_function(xi,
                                                        Si))
    sord <- order(sdist)
    s1 <- sord[[1]]
    s2 <- sord[[2]]
    Age[s1, ] <- Age[s1, ] + 1
    Age[, s1] <- Age[, s1] + 1
    S_meta$error[[s1]] <- S_meta$error[[s1]] + sdist[[s1]]
    S[s1, ] <- move_function(xi, S[s1, ], epsilon_b)
    neighs <- E[[s1]]
    for (n1 in neighs) {
      S[n1, ] <- move_function(xi, S[n1, ], epsilon_n)
    }
    if (is.na(Age[[s1, s2]])) {
      E[[s1]] <- c(E[[s1]], s2)
      E[[s2]] <- c(E[[s2]], s1)
    }
    Age[[s1, s2]] <- 0
    Age[[s2, s1]] <- 0
    edge.nods <- unique(neighs, s2)
    rem <- which(Age[s1, ] > age_max)
    if (length(rem) > 0) {
      Age[s1, rem] <- NA
      Age[rem, s1] <- NA
      E[[s1]] <- setdiff(E[[s1]], rem)
      for (sj in rem) {
        E[[sj]] <- setdiff(E[[sj]], s1)
      }
      s1rem <- c(s1, rem)
      removed.nodes <- s1rem[which(sapply(s1rem, function(i) length(E[s1rem]) ==
                                            0))]
    }
    if (current.iter%%lambda == 0) {
      if (nextr <= max_nodes) {
        r <- nextr
        nextr <- nextr + 1
        p <- which.max(S_meta$error)
        np <- E[[p]]
        q <- np[which.max(S_meta$error[np])]
        S[r, ] <- (S[p, ] + S[q, ])/2
        Age[p, q] <- NA
        Age[q, p] <- NA
        E[[p]] <- c(setdiff(E[[p]], q), r)
        E[[q]] <- c(setdiff(E[[q]], p), r)
        E[[r]] <- c(p, q)
        Age[p, r] <- 0
        Age[r, p] <- 0
        Age[q, r] <- 0
        Age[r, q] <- 0
        Ep <- S_meta$error[[p]]
        Eq <- S_meta$error[[q]]
        S_meta$error[c(p, q, r)] <- c(alpha * Ep, alpha *
                                        Eq, alpha * Ep)
      }
    }
    S_meta$error <- S_meta$error * beta
    if (current.iter %in% make_logs_at) {
      current_log <- list(current.iter = current.iter,
                          S = S, S_meta = S_meta, Age = Age, E = E)
      log[[length(log) + 1]] <- current_log
    }
  }
  nodes <- data.frame(node = seq_len(nrow(S)), S_meta[, 2,
                                                      drop = F])
  node_space <- S
  filt <- !is.na(node_space[, 1])
  nodes <- nodes[filt, , drop = F]
  node_space <- node_space[filt, , drop = F]
  edges <- bind_rows(lapply(seq(2, nrow(S)), function(nj) {
    ni <- E[[nj]]
    ni <- ni[ni < nj]
    if (length(ni) > 0) {
      data.frame(i = ni, j = nj)
    }
    else {
      NULL
    }
  }))
  list(nodes = nodes, node_space = node_space, edges = edges,
       log = log)
}

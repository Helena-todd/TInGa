library(tidyverse)
library(workspace)
library(gng)
library(gganimate)

project("researchgng", "test_gng")

# read trajectory file
traj <- read_trajectory_svg("combined2")

# sample points from density
points <- generate_points(traj$density, num_points = 2000)

# plot points with gold standard
ggplot() +
  geom_point(aes(x, y), traj$gold, colour = "yellow") +
  geom_point(aes(x, y), points) +
  theme_bw()

# fit gng to points
fit <- gng:::gng_r(
  x = as.matrix(points), # make sure x is a matrix
  max_iter = 20000,
  epsilon_b = .05,
  epsilon_n = .001,
  age_max = 200,
  max_nodes = 30,
  lambda = 200,
  alpha = .5,
  beta = .99,
  verbose = TRUE,
  make_logs_at = unique(as.integer(c(0:10, 10 ^ seq(log10(10), log10(20000), length.out = 5000))))
)

fit <- GNGI_5(
  x = as.matrix(points), # make sure x is a matrix
  max_iter = 20000,
  age_max = 200,
  epsilon_b = .05,
  epsilon_n = .001,
  a_mature = 2000,
  verbose = TRUE,
  make_logs_at = unique(as.integer(c(0:10, 10 ^ seq(log10(10), log10(20000), length.out = 5000)))),
  sigma = sd(as.matrix(points))/2
)

processed_log <-
  map(fit$log, function(log) {
    nodes <-
      data.frame(
        iteration = log$current.iter,
        # log$S_meta,
        # i = seq_len(nrow(log$S)),
        node = factor(rownames(log$S), levels = rownames(log$S)),
        log$S,
        check.names = FALSE
      ) %>%
      filter(!is.na(x)) %>%
      as_data_frame()

    if(is.null(rownames(log$Age))){
      rownames(log$Age) <- paste0("Node", 1:nrow(log$Age))
      colnames(log$Age) <- paste0("Node", 1:ncol(log$Age))
    }
    edges <-
      log$Age %>%
      reshape2::melt(varnames = c("from", "to"), value.name = "age") %>%
      filter(!is.na(age), as.character(from) < as.character(to)) %>%
      as_data_frame() %>%
      mutate(iteration = log$current.iter) %>%
      select(iteration, from, to, age)

    if (nrow(nodes) > 1 && nrow(edges) > 0) {
      edges <- edges %>%
        left_join(nodes %>% select(from = node, from_x = x, from_y = y), by = "from") %>%
        left_join(nodes %>% select(to = node, to_x = x, to_y = y), by = "to")
    }

    list(nodes = nodes, edges = edges)
  })

log_nodes <- map_df(processed_log, ~ .$nodes)
log_edges <- map_df(processed_log, ~ .$edges)

tmpdf <- tibble(
  iteration = sort(unique(c(log_nodes$iteration, log_edges$iteration))),
  x = rep(0, length(iteration))
)
g <-
  ggplot(log_nodes) +
  geom_point(aes(x, y), points) +
  geom_point(aes(x, x), tmpdf, alpha = 0) +
  geom_segment(aes(x = from_x, xend = to_x, y = from_y, yend = to_y, colour = age), log_edges) +
  geom_point(aes(x, y), log_nodes, colour = "darkblue") +#, size = error), log_nodes, colour = "darkblue") +
  labs(title = "Iteration = {current_frame}") +
  scale_colour_distiller(palette = "RdYlBu", limits = c(0, 200)) +
  theme_bw() +
  transition_manual(iteration)
print(g)
anim_save(filename = workspace::result_file("GNGI_amature_20000_sigma_15.gif"))
anim_save(filename = workspace::result_file("GNGI_test_poster.gif"))
anim_save(filename = ("~/Desktop/GNGI_test_poster.gif"))


wanted_iters <- c(2L, 100L, 700L, 9508L)
log_edges_ <- log_edges %>% filter(iteration %in% wanted_iters) %>% mutate(iteration = forcats::fct_inorder(paste0("Iteration ", iteration)))
log_nodes_ <- log_nodes %>% filter(iteration %in% wanted_iters) %>% mutate(iteration = forcats::fct_inorder(paste0("Iteration ", iteration)))
tmpdf_ <- tmpdf %>% filter(iteration %in% wanted_iters) %>% mutate(iteration = forcats::fct_inorder(paste0("Iteration ", iteration)))
g <-
  ggplot(log_nodes_) +
  geom_point(aes(x, y), points, colour = "grey") +
geom_point(aes(x, x), tmpdf_, alpha = 0) +
  geom_segment(aes(x = from_x, xend = to_x, y = from_y, yend = to_y), log_edges_) +
  geom_point(aes(x, y), colour = "darkblue") +
  labs(x = "Component 1", y = "Component 2") +
  theme_bw() +
  theme(panel.grid = element_blank(), axis.text = element_blank(), axis.ticks = element_blank()) +
  facet_wrap(~iteration, nrow = 1)
g

ggsave("~/Desktop/gng_iter.pdf", g, width = 12, height = 4)

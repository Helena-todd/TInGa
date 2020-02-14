#' run_methods
#'
#' @param dataset The scRNAseq dataset, as returned by dynwrap
#' @param method_param_name Name of the method to be used (ex = gng_param())
#' @param parameters The parameters that will be given to the method param
#'
#' @return a list of errors and results from the required GNG version
#' @export

run_methods <- function(dataset, method_param_name, parameters){
  methods_name <- list("GNG_version" = eval(parse(text= method_param_name)))

  dataset_orig <- list(dataset)

  parameters_sets <- list(parameters)

  iterations <- c(1:10)

  combi <- crossing(
    method_name = names(methods_name),
    dataset_i = seq_along(dataset_orig),
    param_i = seq_along(parameters_sets),
    iter_i = iterations
  )

  scores_gng_version <- lapply(seq_len(nrow(combi)), function(i){
    settings <- combi %>% extract_row_to_list(i)
    dataset <- dataset_orig[[settings$dataset_i]]
    method <- methods_name[[settings$method_name]]
    parameters <- parameters_sets[[settings$param_i]]
    print(paste0("Applying ", method_param_name, ", going over iteration ", i))

    eval <-
      dyneval::evaluate_ti_method(
        dataset = dataset,
        method = method,
        parameters = parameters,
        metrics = c("correlation", "him", "featureimp_wcor", "F1_branches")
      )

    # plot_dimred(eval$models[[1]])
    eval$summary <- eval$summary %>% mutate(
      i,
      mean_score = dynutils::calculate_geometric_mean(
        correlation,
        him,
        featureimp_wcor,
        F1_branches
      )
    )

    eval
  })

  errors_gng_version <- map(scores_gng_version, function(score_iter){
    error <- score_iter$summary$error
  })

  return(list(errors_gng_version = errors_gng_version,
              scores_gng_version = scores_gng_version))
}

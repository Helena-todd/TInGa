#' generate_parameter_combinations
#'
#' @param default_parameters list of the default parameters
#' @param parameters_to_tweak the parameter to tweak
#'
#' @return a matrix containing the possible combinations of the parameters
#' @export
generate_parameter_combinations <- function(default_parameters, parameters_to_tweak){
  change_one_param <- function(default_params = default_parameters,
                               param_to_tweak = parameters_to_tweak){
    changed_params <- lapply(seq_along(unlist(param_to_tweak)), function(tweak_iter){
      param_set <- default_params
      param_set[names(param_to_tweak)] <- unlist(param_to_tweak)[tweak_iter]
      unlist(param_set)
    })
    changed_params <- as.data.frame(do.call(rbind, changed_params))
    changed_params
  }

  list_params <- lapply(seq_along(params_to_tweak = parameters_to_tweak), function(param_id){
    param <- params_to_tweak[param_id]
    change_one_param(default_params, param)
  })

  combi <- as.data.frame(do.call(rbind, list_params))
  combi <- unique(combi)
  return(combi)
}

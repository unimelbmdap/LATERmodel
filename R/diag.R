

sweep_fitted_params <- function(data, fit) {

  data <- data.frame(
    name = data$name,
    promptness = data$promptness,
    name_factor = factor(data$name)
  )

  data <- set_data_param_indices(data = data, fit_info = fit)

  modified_fit <- rlang::duplicate(x = fit)

  param_ranges <- list(
    mu = seq(from = 0.1, to = 10, length.out = 100),
    sigma = 10 ** seq(from = log10(0.1), to = log10(3), length.out = 100),
    sigma_e = 10 ** seq(from = log10(0.1), to = log10(9), length.out = 100),
    k = seq(from = 0.1, to = 10, length.out = 100)
  )

  param_nll <- list()

  for (param_num in 1:modified_fit$n_params) {

    if (param_num <= modified_fit$n_a) {
      param_type <- ifelse(modified_fit$intercept_form, "k", "mu")
    }
    else if (param_num <= (modified_fit$n_a + modified_fit$n_sigma)) {
      param_type <- "sigma"
    }
    else {
      param_type <- "sigma_e"
    }

    curr_param_info <- list(x = param_ranges[[param_type]], y = c())

    curr_param_nll = list()

    params <- rlang::duplicate(modified_fit$optim_result$par)

    for (param_val in param_ranges[[param_type]]) {
      params[param_num] <- param_val

      labelled_params <- unpack_params(
        params = params,
        n_a = modified_fit$n_a,
        n_sigma = modified_fit$n_sigma,
        n_sigma_e = modified_fit$n_sigma_e
      )

      labelled_params <- append(
        labelled_params,
        convert_a_to_mu_and_k(
          a = labelled_params$a,
          sigma = labelled_params$sigma,
          intercept_form = modified_fit$intercept_form
        )   
      )

      modified_fit$fitted_params <- labelled_params

      loglike <- calc_loglike(data = data, fit_info = modified_fit)

      #curr_param_nll = c(curr_param_nll, -1 * loglike)
      curr_param_info$y <- c(curr_param_info$y, -1 * loglike)

    }

    param_nll[[param_num]] <- curr_param_info

  }
  return(param_nll)
  return(c(param_ranges, curr_param_nll))

}

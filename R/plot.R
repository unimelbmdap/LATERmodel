#' Plot reaction times and LATER model fit in reciprobit axes
#'
#' @param plot_data A dataframe with columns: `time`, `name`, `promptness`,
#' and `e_cdf`. Optionally, there may be a `color` column, which contains
#' hex values, one unique hex value per named dataset
#' @param fit_params A dataframe with one row for each named dataset and columns
#' equal to the LATER model parameters returned by `fit_data$named_fit_params`
#' @param time_breaks Desired tick marks on the x axis, expressed in
#' promptness (1/s)
#' @param probit_breaks Desired tick marks on the y axis in probit space
#' @param z_breaks Desired tick marks on secondary y axis, in z values
#' @param xrange Desired range for the x axis, in promptness (1/s)
#' @param yrange Desired range for the y axis, in cumulative probability space
#'
#' @returns A reciprobit plot with the cumulative probability distribution of
#' the reaction times
#' @export
#'
#' @importFrom rlang .data
#'
#' @examples
#' \donttest{
#' data <- rbind(
#'   data.frame(name = "test", time = 1000/rnorm(100, 3, 1)),
#'   data.frame(name = "test_2", time = 1000/rnorm(100, 4, 1))
#' ) |> dplyr::filter(time > 0)
#' data <- prepare_data(data)
#' fit_params <- individual_later_fit(data)
#' reciprobit_plot(data, fit_params)
#' }
reciprobit_plot <- function(
    plot_data,
    fit_params = NULL,
    time_breaks = c(0.1, 0.2, 0.3, 0.5, 1),
    probit_breaks = c(0.1, 1, 5, 10, 20, 50, 80, 90, 95, 99, 99.9),
    z_breaks = c(-2, -1, 0, 1, 2),
    xrange = NULL,
    yrange = NULL) {
  color_brewer_colors <- c(
    "#1b9e77",
    "#d95f02",
    "#7570b3",
    "#e7298a",
    "#66a61e",
    "#e6ab02",
    "#a6761d",
    "#666666"
  )

  # Make sure plot_data is sorted by the name column, otherwise the color labels
  # (set with unique(plot_data$name)) do not correspond to the dataset names
  plot_data <- dplyr::arrange(plot_data, .data$name)

  if ("color" %in% names(plot_data)) {
    colors <- c(unique(plot_data$color), color_brewer_colors)
  } else {
    colors <- color_brewer_colors
  }

  # Remove points not defined in probit space
  plotting_data <- dplyr::filter(plot_data, .data$e_cdf > 0)

  # If yrange or xrange is not specified, then use the maximum and minimum
  # values present in the data
  if (is.null(yrange)) {
    yrange <- c(min(1 - plotting_data$e_cdf), max(1 - plotting_data$e_cdf))
  }
  if (is.null(xrange)) {
    xrange <- c(max(plotting_data$promptness), min(plotting_data$promptness))
  } else if (xrange[1] < xrange[2]) {
    xrange <- rev(xrange)
  }

  # Prepare for deprecation in `trans` argument after ggplot 3.5.0
  if (utils::packageVersion("ggplot2") < "3.5.0") {
    trans_arg <- list(trans = stats::qnorm)
  } else {
    trans_arg <- list(transform = stats::qnorm)
  }

  plot <- plotting_data |>
    ggplot2::ggplot(ggplot2::aes(
      x = .data$promptness,
      y = 1. - .data$e_cdf,
      colour = .data$name
    )) +
    ggplot2::geom_point() +
    ggplot2::scale_x_reverse(
      # Main axis
      name = "Latency (s)",
      breaks = 1 / time_breaks,
      labels = formatC(time_breaks, digits = 2, format = "g"),
      minor_breaks = NULL,
      # Secondary axis
      sec.axis = ggplot2::dup_axis(
        name = "Promptness (1/s)",
        labels = formatC(1 / time_breaks, digits = 2, format = "g")
      )
    ) +
    ggplot2::scale_y_continuous(
      # Main axis
      name = "Cumulative percent probability",
      trans = "probit", breaks = probit_breaks / 100,
      labels = probit_breaks,
      minor_breaks = stats::pnorm(z_breaks),
      # Secondary axis
      sec.axis =
        do.call(
          ggplot2::sec_axis,
          c(
            list(
              name = "Z-score",
              breaks = z_breaks
            ),
            trans_arg
          )
        )
    ) +
    ggplot2::coord_cartesian(
      xlim = xrange,
      ylim = yrange
    ) +
    ggplot2::scale_color_manual(
      values = as.character(colors),
      labels = unique(plot_data$name)
    ) +
    ggplot2::theme_minimal() +
    ggplot2::labs(color = "") +
    ggplot2::theme(
      panel.grid.minor = ggplot2::element_line(linetype = 2)
    )

  if (!is.null(fit_params)) {
    if (!"name" %in% colnames(fit_params)) {
      fit_params <- fit_params |>
        tibble::rownames_to_column(var = "name") |>
        dplyr::mutate(
          name = factor(.data$name, levels = unique(plot_data$name))
        )
    }

    fit_params <- dplyr::arrange(fit_params, .data$name)

    if (
      !isTRUE(
        all.equal(
          as.character(unique(fit_params$name)),
          as.character(unique(plot_data$name))
        )
      )
    ) {
      rlang::abort(
        "The names of the datasets in plot_data and fit_params do not match, or
        have different orders."
      )
    }

    x_eval <- seq(
      xrange[2],
      xrange[1],
      length.out = 100
    )

    plot_fit <- fit_params |>
      dplyr::reframe(
        x = x_eval,
        fit = model_cdf(
          x_eval,
          later_mu = .data$mu,
          later_sd = .data$sigma,
          early_sd = if ("sigma_e" %in% names(fit_params)) {
            .data$sigma_e
          } else {
            NULL
          }
        ),
        .by = "name"
      ) |>
      dplyr::filter(1 - .data$fit >= yrange[1] & 1 - .data$fit <= yrange[2])

    plot <- plot +
      ggplot2::geom_line(
        data = plot_fit,
        ggplot2::aes(x = .data$x, y = 1. - .data$fit, colour = .data$name),
        linewidth = 0.5
      )
  }

  plot
}

#' Fit individual LATER model to each dataset in a dataframe of datasets
#'
#' @param df A dataframe with columns: `time`, `name`, `promptness`, and `e_cdf`
#' @param with_early_component If `TRUE`, the model contains a second 'early'
#'  component that is absent when `FALSE` (the default).
#' @param fit_criterion String indicating the criterion used to optimise the
#'  fit by seeking its minimum.
#'   * `ks`: Kolmogorov-Smirnov statistic.
#'   * `neg_loglike`: Negative log-likelihood.
#' @param jitter_settings Settings for running the fitting multiple times with
#'   randomly-generated offsets ('jitter') applied to the starting estimates.
#'   * `n`: How many jitter iterations to run (default of 7).
#'   * `prop`: The maximum jitter offset, as a proportion of the start
#'   value (default of 0.5).
#'   * `seed`: Seed for the random jitter generator (default is unseeded).
#'   * `processes`: Maximum number of CPU processes that can be used (default
#'   is 2).
#'
#' @returns A dataframe with one row for each named dataset in `df` and columns
#' equal to the LATER model parameters returned by fit_data$named_fit_params
#' @export
#'
#' @examples
#' \donttest{
#' data <- rbind(
#'   data.frame(name = "test", promptness = rnorm(100, 3, 1)),
#'   data.frame(name = "test_2", promptness = rnorm(100, 1, 1))
#' )
#' fit_params <- individual_later_fit(data)
#' }
individual_later_fit <- function(
    df,
    with_early_component = FALSE,
    fit_criterion = "likelihood",
    jitter_settings = list(n = 7, prop = 0.5, seed = NA, processes = 2)) {
  df |>
    dplyr::group_by(.data$name) |>
    dplyr::group_modify(
      ~ extract_fit_params_and_stat(
        .x,
        with_early_component = with_early_component,
        fit_criterion = fit_criterion,
        jitter_settings = jitter_settings
      ),
      .keep = TRUE
    ) |>
    dplyr::ungroup()
}

extract_fit_params_and_stat <- function(
    data,
    with_early_component,
    fit_criterion,
    jitter_settings) {
  this_list <- fit_data(
    data,
    with_early_component = with_early_component,
    fit_criterion = fit_criterion,
    jitter_settings = jitter_settings
  )

  df <- data.frame(
    this_list$named_fit_params,
    fit_criterion = this_list$fit_criterion,
    stat = this_list$optim_result$value,
    loglike = this_list$loglike,
    aic = this_list$aic
  )

  df
}

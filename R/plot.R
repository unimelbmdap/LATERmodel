#' Plot reaction times and LATER model fit in reciprobit axes
#'
#' @param plot_data A dataframe with columns: `time`, `color`, `name`,
#' `promptness`, and `e_cdf`; one color per name (participant)
#' @param fit_params A dataframe with one row for each named dataset and columns
#' equal to the LATER model parameters returned by `fit_data$named_fit_params`
#' @param time_breaks Desired tick marks on the x axis, expressed in Latency (s)
#' @param probit_breaks Desired tick marks on the y axis in probit space
#' @param z_breaks Desired tick marks on secondary y axis, in z values
#' @param xrange Desired range for the x axis, in seconds
#' @param yrange Desired range for the y axis, in probit space
#'
#' @return A reciprobit plot with the cumulative probability distribution of
#' the reaction times
#' @export
#'
#' @importFrom rlang .data
#'
#' @examples
#' a <- dplyr::filter(
#'   carpenter_williams_1995, participant == "a",
#'   condition == "p95" | condition == "p05"
#' )
#' df <- prepare_data(a)
#' fit_params <- individual_later_fit(df, with_early_component = TRUE)
#' reciprobit_plot(df, fit_params)
reciprobit_plot <- function(
    plot_data,
    fit_params = NULL,
    time_breaks = c(0.1, 0.2, 0.3, 0.5, 1),
    probit_breaks = c(1, 5, 10, 20, 50, 80, 90, 95, 99),
    z_breaks = c(-2, -1, 0, 1, 2),
    xrange = NULL,
    yrange = NULL) {
  plot <- dplyr::filter(plot_data, .data$e_cdf > 0) |>
    ggplot2::ggplot(ggplot2::aes(
      x = .data$promptness,
      y = 1. - .data$e_cdf,
      colour = .data$color
    )) +
    ggplot2::geom_point() +
    ggplot2::scale_x_reverse(
      # Main axis
      name = "Latency(s)",
      breaks = 1 / time_breaks,
      labels = time_breaks,
      minor_breaks = NULL,
      # Secondary axis
      sec.axis = ggplot2::dup_axis(
        name = "Promptness (1/s)",
        labels = formatC(1 / time_breaks, digits = 2)
      )
    ) +
    ggplot2::coord_cartesian(
      xlim = xrange,
      ylim = yrange
    ) +
    ggplot2::scale_y_continuous(
      # Main axis
      name = "Cumulative percent probability",
      trans = "probit", breaks = probit_breaks / 100,
      labels = probit_breaks,
      minor_breaks = stats::pnorm(z_breaks),
      # Secondary axis
      sec.axis = ggplot2::sec_axis(
        trans = stats::qnorm,
        name = "Z-score",
        breaks = z_breaks
      )
    ) +
    ggplot2::scale_color_manual(
      values = as.character(unique(plot_data$color)),
      labels = unique(plot_data$name)
    ) +
    ggplot2::theme_minimal() +
    ggplot2::labs(color = "") +
    ggplot2::theme(
      panel.grid.minor = ggplot2::element_line(linetype = 2)
    )

  if (!is.null(fit_params)) {
    x_eval <- seq(
      min(plot_data$promptness),
      max(plot_data$promptness),
      length.out = 100
    )

    plot_fit <- fit_params |>
      dplyr::reframe(
        x = x_eval,
        fit = model_cdf(
          x_eval,
          later_mu = .data$mu,
          later_sd = .data$sigma,
          early_sd = .data$sigma_e
        )
      )

    plot <- plot +
      ggplot2::geom_line(
        data = plot_fit,
        ggplot2::aes(x = .data$x, y = 1. - .data$fit, colour = .data$color),
        linewidth = 0.5
      )
  }

  plot
}

#' Fit individual LATER model to each dataset in a dataframe of datasets
#'
#' @param df A dataframe with columns: `time`, `color`, `name`,
#' `promptness`, and `e_cdf`; one color per name (participant)
#' @param with_early_component If `TRUE`, the model contains a second 'early'
#'  component that is absent when `FALSE` (the default).
#'
#' @return A dataframe with one row for each named dataset in `df` and columns
#' equal to the LATER model parameters returned by fit_data$named_fit_params
#' @export
#'
#' @examples
#' a <- dplyr::filter(carpenter_williams_1995, participant == "a")
#' df <- prepare_data(a)
#' fit_params <- individual_later_fit(df)
individual_later_fit <- function(df, with_early_component = FALSE) {
  df |>
    dplyr::group_by(.data$name, .data$color) |>
    dplyr::group_modify(
      ~ fit_data(
        .x,
        with_early_component = with_early_component
      )$named_fit_params,
      .keep = TRUE
    )
}
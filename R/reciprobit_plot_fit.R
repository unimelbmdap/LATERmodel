#' Plot reaction times and LATER model fit in reciprobit axes
#'
#' @param plot_data A dataframe with columns: `time`, `color`, `name`,
#' `promptness`, and `e_cdf`; one color per name (participant)
#' @param fit_info A dataframe with the results of LATER model fitting of the
#' input data
#'
#' @return A reciprobit plot with the cumulative probability distribution of
#' the reaction times and LATER model fit
#' @export
#'
#' @importFrom rlang .data
#'
#' @examples
#' df <- prepare_data(rt2)
#' fit_info <- fit_data(df)
#' reciprobit_plot_fit(df,fit_info)
reciprobit_plot_fit <- function(plot_data, fit_info) {
  time_breaks <- c(0.1, 0.2, 0.3, 0.5, 1)
  probit_breaks <- c(0.1, 1, 5, 10, 20, 50, 80, 90, 95, 99, 99.9)
  z_breaks <- c(-2, -1, 0, 1, 2)


  gg <- ggplot2::ggplot(
    subset(plot_data, e_cdf > 0.),
    ggplot2::aes(x = .data$promptness, y = 1. - .data$e_cdf, colour = .data$color)
  )
  gg <- gg + ggplot2::geom_point()

  gg <- gg + ggplot2::scale_x_reverse(
      # Main axis
      name = "Latency(s)",
      breaks = 1 / time_breaks,
      labels = time_breaks,
      minor_breaks = NULL,
      limits = c(max(plot_data$promptness), min(plot_data$promptness)),
      # Secondary axis
      sec.axis = ggplot2::dup_axis(
        name = "Promptness (1/s)",
        labels = formatC(1 / time_breaks, digits = 2)
      )
    )

    gg <- gg + ggplot2::scale_y_continuous(
      # Main axis
      name = "Cumulative percent probability",
      trans = "probit", breaks = probit_breaks / 100,
      labels = probit_breaks,
      minor_breaks = stats::pnorm(z_breaks),
      limits = c(0.01/100., 99.9/100.),
      # Secondary axis
      sec.axis = ggplot2::sec_axis(
        trans = stats::qnorm,
        name = "Z-score",
        breaks = z_breaks
      )
    )

    # Unique seems to preserve order of appearance, whereas level doesn't
    gg <- gg + ggplot2::scale_color_manual(
      values = as.character(unique(plot_data$color)),
      labels = unique(plot_data$name)
    )
    gg <- gg + ggplot2::theme_minimal()
    gg <- gg + ggplot2::labs(color = "")
    gg <- gg + ggplot2::theme(
      panel.grid.minor = ggplot2::element_line(linetype = 2)
    )

    names <- unique(plot_data$name)
    for (n in names) {
      data_filter <- subset(plot_data, name == n)

      x_eval <- seq(min(plot_data$promptness), max(plot_data$promptness),
                    length.out=100)

      pl <- LATERmodel::model_cdf(x_eval, later_mu = fit_info$named_fit_params[n, "mu"],
                                  later_sd = fit_info$named_fit_params[n, "sigma"],
                                  early_sd = fit_info$named_fit_params[n, "sigma_e"])

      model_plot = data.frame(x = x_eval, y = pl)

      color = toString(data_filter$color[1])

      gg <- gg + ggplot2::geom_line(data=model_plot, ggplot2::aes(x = x, y = 1. - y),
                                    colour=color, linewidth = 0.5)

    }

    gg

}

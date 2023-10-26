#' Plot reaction times in reciprobit axes
#'
#' @param plot_data A dataframe with columns: `time`, `color`, `name`,
#' `promptness`, and `e_cdf`; one color per name (participant)
#'
#' @return A reciprobit plot with the cumulative probability distribution of
#' the reaction times
#' @export
#'
#' @importFrom rlang .data
#'
#' @examples
#' df <- prepare_data(rt2)
#' reciprobit_plot(df)
reciprobit_plot <- function(plot_data) {
  time_breaks <- c(0.1, 0.2, 0.3, 0.5, 1)
  probit_breaks <- c(0.1, 1, 5, 10, 20, 50, 80, 90, 95, 99, 99.9)
  z_breaks <- c(-2, -1, 0, 1, 2)


  ggplot2::ggplot(
    subset(plot_data, e_cdf > 0.),
    ggplot2::aes(x = .data$promptness, y = 1. - .data$e_cdf, colour = .data$color)
  ) +
    ggplot2::geom_point() +
    ggplot2::scale_x_reverse(
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
    ) +
    ggplot2::scale_y_continuous(
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
    ) +
    # Unique seems to preserve order of appearance, whereas level doesn't
    ggplot2::scale_color_manual(
      values = as.character(unique(plot_data$color)),
      labels = unique(plot_data$name)
    ) +
    ggplot2::theme_minimal() +
    ggplot2::labs(color = "") +
    ggplot2::theme(
      panel.grid.minor = ggplot2::element_line(linetype = 2)
    )
}

#' Plot reaction times in reciprobit axes
#'
#' @param plot_data A dataframe with columns: `times`, `color`, `name`, `promptness`,
#' and `e_cdf`; one color per name (participant)
#'
#' @return A reciprobit plot with the cumulative probability distribution of the reaction times
#' @export
#'
#' @importFrom rlang .data
#'
#' @examples
#' df <- prepare_data(rt2)
#' reciprobit_plot(df)

reciprobit_plot <- function(plot_data){

  time_breaks   <- c(0.1,0.2,0.3,0.5,1)
  probit_breaks <- c(1,5,10,20,50,80,90,95,99)
  Z_breaks      <- c(-2, -1, 0, 1, 2)


  ggplot2::ggplot(plot_data, ggplot2::aes(x=.data$promptness, y=.data$e_cdf, colour = .data$color)) +
    ggplot2::geom_point() +
    ggplot2::scale_x_reverse(
      # Main axis
      name = "Latency(s)", breaks = 1/time_breaks, labels=time_breaks, minor_breaks = NULL,
      # Secondary axis
      sec.axis = ggplot2::dup_axis(name="Promptness (1/s)", labels = formatC(1/time_breaks, digits=2))
    ) +
    ggplot2::scale_y_continuous(
      # Main axis
      name = "Cumulative percent probability", trans = 'probit', breaks = probit_breaks/100, labels = probit_breaks, minor_breaks = stats::pnorm(Z_breaks),
      # Secondary axis
      sec.axis = ggplot2::sec_axis(trans = stats::qnorm, name="Z-score", breaks = Z_breaks)
    ) +
    # Unique seems to preserve order of appearance, whereas level doesn't
    ggplot2::scale_color_manual(values = unique(plot_data$color), labels = unique(plot_data$name)) +
    ggplot2::theme_minimal() +
    ggplot2::labs(color = "") +
    ggplot2::theme(
      panel.grid.minor = ggplot2::element_line(linetype = 2)
    )

}

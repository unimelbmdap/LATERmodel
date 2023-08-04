#' Plot reaction times in reciprobit axes
#'
#' @param rt vector of reaction times
#'
#' @return A reciprobit plot with the cumulative probability distribution of the reaction times
#' @export
#'
#' @importFrom rlang .data
#'
#' @examples
#' reciprobit_plot(rt)

reciprobit_plot <- function(rt){

  time_breaks   <- c(0.1,0.2,0.3,0.5,1)
  probit_breaks <- c(1,5,10,20,50,80,90,95,99)
  Z_breaks      <- c(-2, -1, 0, 1, 2)

  plot_data <- data.frame(times = sort(rt/1000)) %>%
    dplyr::mutate(promptness = 1/.data$times, e_cdf = sort(stats::ecdf(.data$times)(.data$times))) %>%
    dplyr::filter(.data$e_cdf < 1)

  ggplot2::ggplot(plot_data, ggplot2::aes(x=.data$promptness, y=.data$e_cdf)) +
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
    ggplot2::theme_minimal()

}


#' Prepares data for reciprobit plot
#'
#' @param rt Vector of reaction times for a single participant, or a dataframe
#' with a column called `time` with the reaction times and another called
#' `color` that contains one hexadecimal color code for each participant.
#' Optionally may contain a column `name` to use as labels for each dataset.
#' @param time_unit Units of the reaction times in rt_vector,
#' must be one of "ms", "ds", or "s".
#'
#' @return A dataframe with columns: `time`, `color`, `name`, `promptness`, and `e_cdf`.
#'
#' @export
#'
#' @examples
#' df <- prepare_data(rt)
prepare_data <- function(rt, time_unit = "ms") {
  if(typeof(rt) == "double"){
    plot_data <- data.frame(time = convert_to_seconds(rt, time_unit = time_unit)) %>%
      dplyr::mutate(color = "#1B9E77")
  } else if(typeof(rt) == "list") {
    plot_data <- rt %>%
      dplyr::mutate(time = convert_to_seconds(.data$time, time_unit = time_unit))
  }

  # If no names, create dataset names from unique colors
  if(!"name" %in% colnames(plot_data)){
    plot_data <- transform(plot_data, name = match(plot_data$color, unique(plot_data$color)))
  }

  plot_data %>%
    dplyr::arrange(.data$color, .data$time) %>%
    dplyr::group_by(.data$color) %>%
    dplyr::mutate(
      promptness = 1/.data$time,
      e_cdf = stats::ecdf(.data$time)(.data$time)
    ) %>%
    dplyr::filter(.data$e_cdf < 1)

}

#' Convert reaction time vector to seconds. Used only internally, so not exported
#'
#' @param rt_vector Vector of reaction times, with type 'double'
#' @param time_unit Units of rt_vector, must be one of "ms", "ds", or "s"
#'
#' @return rt_vector in seconds

convert_to_seconds <- function(rt_vector, time_unit = "ms") {

  if(time_unit == "ms"){
    rt_vector/1000
  } else if(time_unit == "ds") {
    10*rt_vector
  } else if(time_unit == "s") {
    rt_vector
  } else {
    rlang::abort('`time_unit` must be one of "ms", "ds", or "s".')
  }

}

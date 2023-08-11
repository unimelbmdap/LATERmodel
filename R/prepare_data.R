
#' Prepares data for reciprobit plot
#'
#' @param rt Vector of reaction times for a single participant, or a dataframe
#' with a column called `times` with the reaction times and another called
#' `color` that contains one hexadecimal color code for each participant.
#' Optionally may contain a column `name` to use as labels for each dataset.
#' @param time_units Units of the reaction times in rt_vector,
#' must be one of "ms", "ds", or "s".
#'
#' @return A dataframe with columns: `times`, `color`, `name`, `promptness`, and `e_cdf`.
#'
#' @export
#'
#' @examples
#' df <- prepare_data(rt)
prepare_data <- function(rt, time_units = "ms") {
  if(typeof(rt) == "double"){
    plot_data <- data.frame(times = convert_to_seconds(rt, time_units = time_units)) %>%
      dplyr::mutate(color = "#1B9E77")
  } else if(typeof(rt) == "list") {
    plot_data <- rt %>%
      dplyr::mutate(times = convert_to_seconds(.data$times, time_units = time_units))
  }

  # If no names, create dataset names from unique colors
  if(!"name" %in% colnames(plot_data)){
    plot_data <- transform(plot_data, name = match(plot_data$color, unique(plot_data$color)))
  }

  plot_data %>%
    dplyr::arrange(.data$color, .data$times) %>%
    dplyr::group_by(.data$color) %>%
    dplyr::mutate(
      promptness = 1/.data$times,
      e_cdf = stats::ecdf(.data$times)(.data$times)
    ) %>%
    dplyr::filter(.data$e_cdf < 1)

}

#' Convert reaction time vector to seconds. Used only internally, so not exported
#'
#' @param rt_vector Vector of reaction times, with type 'double'
#' @param time_units Units of rt_vector, must be one of "ms", "ds", or "s"
#'
#' @return rt_vector in seconds

convert_to_seconds <- function(rt_vector, time_units = "ms") {

  if(time_units == "ms"){
    rt_vector/1000
  } else if(time_units == "ds") {
    10*rt_vector
  } else if(time_units == "s") {
    rt_vector
  } else {
    rlang::abort('`time_units` must be one of "ms", "ds", or "s".')
  }

}

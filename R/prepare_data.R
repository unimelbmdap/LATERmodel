#' Prepares data for reciprobit plot
#'
#' @param rt Vector of reaction times for a single participant, or a dataframe
#' containing a column called `time` with the reaction times and optional other
#' columns:
#' - a column called `name` with a unique label for each dataset
#' - a column called `participant` with a unique id per participant and another
#' called `condition` with a unique label for each condition. In this case the
#' `name` for each dataset will be constructed as
#' `participant`+`name_separator`+`condition`.
#' - a column called `color` that contains one hexadecimal color code for each
#' dataset. In this case the `name` for each dataset will be set to be the name
#' of the color.
#' @param time_unit Units of the reaction times in rt_vector,
#' must be one of "ms", "ds", or "s".
#' @param name_separator If the `rt` dataframe does not contain a `name` column,
#' but does have `particpant` and `condition` columns, the `name` for each
#' dataset will be constructed as `participant`+`name_separator`+`condition`.
#'
#' @return A dataframe with columns: `time`, `color`, `name`, `promptness`,
#' and `e_cdf`.
#'
#' @export
#'
#' @examples
#' df <- prepare_data(carpenter_williams_1995)
prepare_data <- function(rt, time_unit = "ms", name_separator = "_") {
  if (typeof(rt) == "double") {
    plot_data <- data.frame(
      time = convert_to_seconds(rt, time_unit = time_unit)
    ) |>
      dplyr::mutate(color = "#1B9E77")
  } else if (typeof(rt) == "list") {
    plot_data <- rt |>
      dplyr::mutate(
        time = convert_to_seconds(.data$time, time_unit = time_unit)
      )
  }

  # If no name column, create dataset names from unique colors or from
  # participant and condition
  if (!"name" %in% colnames(plot_data)) {
    if ("color" %in% colnames(plot_data)) {
      plot_data <- transform(
        plot_data,
        name = match(plot_data$color, unique(plot_data$color))
      )
    } else {
      plot_data <- plot_data |>
        dplyr::mutate(
          name = as.factor(paste(.data$participant, .data$condition, sep = name_separator))
        )
    }
  }

  plot_data |>
    dplyr::arrange(.data$name, .data$time) |>
    dplyr::group_by(.data$name) |>
    dplyr::mutate(
      promptness = 1 / .data$time,
      e_cdf = promptness_ecdf(.data$promptness)$y
    ) |>
    dplyr::select(-dplyr::any_of(c("participant", "condition")))
}


#' Convert reaction time vector to seconds. Used only internally.
#'
#' @param rt_vector Vector of reaction times, with type 'double'
#' @param time_unit Units of rt_vector, must be one of "ms", "ds", or "s"
#'
#' @return rt_vector in seconds
#' @noRd
convert_to_seconds <- function(rt_vector, time_unit = "ms") {
  if (time_unit == "ms") {
    rt_vector / 1000
  } else if (time_unit == "ds") {
    10 * rt_vector
  } else if (time_unit == "s") {
    rt_vector
  } else {
    rlang::abort('`time_unit` must be one of "ms", "ds", or "s".')
  }
}

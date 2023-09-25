#' Prepares data for reciprobit plot
#'
#' @param rt Vector of reaction times for a single participant, or a dataframe
#' with a column called `time` with the reaction times and another called
#' `color` that contains one hexadecimal color code for each participant.
#' Optionally may contain a column `name` to use as labels for each dataset.
#' @param time_unit Units of the reaction times in rt_vector,
#' must be one of "ms", "ds", or "s".
#'
#' @return A dataframe with columns: `time`, `color`, `name`, `promptness`,
#' and `e_cdf`.
#'
#' @export
#'
#' @examples
#' df <- prepare_data(rt)
prepare_data <- function(rt, time_unit = "ms") {
  if (typeof(rt) == "double") {
    plot_data <- data.frame(
      time = convert_to_seconds(rt, time_unit = time_unit)
    ) %>%
      dplyr::mutate(color = "#1B9E77")
  } else if (typeof(rt) == "list") {
    plot_data <- rt %>%
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
      plot_data <- plot_data %>%
        dplyr::mutate(
          name = as.factor(paste(.data$participant, .data$condition, sep = "_"))
        )
      plot_data <- add_colors(plot_data)
    }
  }

  plot_data %>%
    dplyr::arrange(.data$color, .data$time) %>%
    dplyr::group_by(.data$color) %>%
    dplyr::mutate(
      promptness = 1 / .data$time,
      e_cdf = stats::ecdf(.data$time)(.data$time)
    ) %>%
    dplyr::filter(.data$e_cdf < 1)
}

#' If colors are not defined, add colors from Color Brewer, up to 8
#'
#' @param df Dataframe with a column called `name` that has a unique name for
#' each dataset
#'
#' @return A dataframe with an extra column called `color` that has a unique
#' color for each dataset
#' @noRd
add_colors <- function(df) {
  if (length(unique(df$name)) > 8) {
    rlang::abort(
      "Plotting is implemented for 8 datasets or fewer at the moment."
    )
  }
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
  df %>% dplyr::mutate(
    color = factor(
      color_brewer_colors[as.numeric(as.factor(.data$name))],
      levels = color_brewer_colors
    )
  )
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

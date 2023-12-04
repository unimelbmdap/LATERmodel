#' Apply two-sample KS test to all pairs of datasets contained within a
#' dataframe
#'
#' @param df A dataframe of datasets with columns: `name` and `time`, one
#' unique `name` per dataset
#' @param correct_multiple_comparisons If `TRUE`, an adjustment will be made
#' to the p-values based on Holm, 1979, A simple sequentially rejective
#' multiple test procedure
#'
#' @return A dataframe with columns `name1`, `name2`, `D`, and `p-value`
#' @export
#'
#' @importFrom utils combn
#'
#' @examples
#' data <- prepare_data(dplyr::filter(
#' carpenter_williams_1995,
#' participant == "b"
#' ))
#' ks_compare(data)
ks_compare <- function(df, correct_multiple_comparisons = TRUE) {
  dataset_names <- sort(unique(df$name))

  # Calculate KS test results and store in a long-format dataframe
  combinations <- combn(dataset_names, 2, simplify = FALSE)
  ks_results <- lapply(combinations, function(pair) {
    name1 <- pair[1]
    name2 <- pair[2]

    time1 <- df |>
      dplyr::filter(.data$name == name1) |>
      dplyr::pull(.data$time)
    time2 <- df |>
      dplyr::filter(.data$name == name2) |>
      dplyr::pull(.data$time)

    result <- data.frame(
      name1 = name1,
      name2 = name2
    ) |>
      dplyr::bind_cols(ks_test(time1, time2))

    return(result)
  }) |>
    dplyr::bind_rows()

  ks_results$name1 <- factor(ks_results$name1, levels = rev(dataset_names))
  ks_results$name2 <- factor(ks_results$name2, levels = dataset_names)

  if (correct_multiple_comparisons) {
    ks_results$p_value <- stats::p.adjust(ks_results$p_value, method = "holm")
  }

  return(ks_results)
}

#' Create a heatmap to visualise if there is not enough evidence to reject the
#' null hypothesis that two datasets come from the same underlying distribution
#'
#' @param ks_results A dataframe with columns `name1`, `name2`, `D`, and
#' `p-value`, obtained using the function ks_compare
#'
#' @return A heatmap plot with all paired comparisons
#' @export
#'
#' @examples
#' data <- prepare_data(dplyr::filter(
#' carpenter_williams_1995,
#' participant == "b"
#' ))
#' ks_results <- ks_compare(data)
#' ks_heatmap(ks_results)
ks_heatmap <- function(ks_results) {
  # Create the heatmap
  ks_results |>
    dplyr::mutate(pval = dplyr::case_when(
      p_value < 0.001 ~ "p<0.001",
      p_value < 0.01 ~ "p<0.01",
      p_value < 0.05 ~ "p<0.05",
      TRUE ~ "p\u22650.05"
    )) |>
    ggplot2::ggplot(
      ggplot2::aes(x = .data$name2, y = .data$name1, fill = .data$pval)
    ) +
    ggplot2::geom_raster() +
    ggplot2::geom_text(
      ggplot2::aes(
        label = paste0(
          "D=",
          format(.data$D, digits = 1),
          "\n",
          ifelse(
            .data$pval=="p<0.001",
            .data$pval,
            paste0("p=", formatC(.data$p_value, digits = 2))
          )
        )
      ),
      hjust = 0.5,
      vjust = 0.5
    ) +
    ggplot2::scale_fill_manual(values = c("p<0.001"="#FFFEE8", "p<0.01"="#FEFDD1", "p<0.05"="#FEFDBA", "p\u22650.05"="#FBF719")) +
    ggplot2::scale_x_discrete(position = "top") +
    ggplot2::theme_minimal() +
    ggplot2::labs(x = "", y = "", fill = "") +
    ggplot2::theme(panel.grid = ggplot2::element_blank())
}

# Function to calculate 2-sample KS test and return D and p value
ks_test <- function(x, y) {
  ks_result <- stats::ks.test(x, y)
  return(data.frame(D = ks_result$statistic, p_value = ks_result$p.value))
}

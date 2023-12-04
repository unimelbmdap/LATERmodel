test_that("ks_compare works as expected", {
  data <- prepare_data(dplyr::filter(
    carpenter_williams_1995,
    participant == "b",
    condition %in% c("p05", "p50", "p95")
  ))

  comparisons <- suppressWarnings({ks_compare(data, correct_multiple_comparisons = TRUE)})

  expect_equal(
    dplyr::filter(
      comparisons,
      dplyr::if_all(
        dplyr::starts_with("name"), ~ . %in% c("b_p05", "b_p50")
      )
    )$D,
    0.66043494
  )

  expect_equal(
    dplyr::filter(
      comparisons,
      dplyr::if_all(
        dplyr::starts_with("name"), ~ . %in% c("b_p05", "b_p95")
      )
    )$D,
    0.80149617
  )

  expect_equal(
    dplyr::filter(
      comparisons,
      dplyr::if_all(
        dplyr::starts_with("name"), ~ . %in% c("b_p50", "b_p95")
      )
    )$D,
    0.27838393
  )
})

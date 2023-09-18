test_that(
  "erf is as expected",
  {
    x <- c(-2, -1, 0, 1, 2)
    # compare against `scipy.special.erf`
    expect_equal(
      erf(x = x),
      c(-0.99532227, -0.84270079, 0, 0.84270079, 0.99532227)
    )
  }
)

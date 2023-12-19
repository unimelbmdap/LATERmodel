
test_that(
  "evidence ratio is as expected",
  {

    # from Motulsky & Christopolous (2004), p. 146
    # if the AICc scores differ by 5.0, then the evidence ratio equals 12.18.
    expect_equal(calc_evidence_ratio(0.0, 5.0), 12.18, tolerance=0.01);
    expect_equal(calc_evidence_ratio(10.0, 15.0), 12.18, tolerance=0.01);

    # If the difference in AICc scores equals 10, then the evidence ratio is 148
    expect_equal(calc_evidence_ratio(0.0, 10.0), 148, tolerance=1);
    expect_equal(calc_evidence_ratio(50.0, 60.0), 148, tolerance=1);

  }
)

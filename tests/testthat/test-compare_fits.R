
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

test_that(
  "comparison is as expected",
  {

    # mock up some dummy fit results
    fit_a = list(aic=10.0);
    fit_b = list(aic=5.0);

    fits = list(a=fit_a, b=fit_b);

    #comparison <- LATERmodel::compare_fits(fits = fits)
    comparison <- compare_fits(fits = fits);

    # first row should be the preferred model
    expect_true(comparison[1, "preferred"]);
    expect_false(comparison[2, "preferred"]);

    expect_equal(comparison$aic, c(5, 10));

    expect_equal(comparison$preferred_rel_fit_delta_aic, c(0, -5));

    expect_equal(
      comparison$preferred_rel_fit_evidence_ratio,
      c(1, calc_evidence_ratio(5, 10))
    );

  }
)

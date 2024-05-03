# LATERmodel 0.2.0

* Changed approach to determine start values for `sigma` and `sigma_e` parameters, using the skewness of the observed promptness distribution to weight the relationship between the parameters and the empirical standard deviation.
* Added the option to perform fitting repeatedly with random offsets (jitters) to the start points (on by default), returning the best of the repeated fits.
* Added a `processes` option in the `jitter_settings` parameter for `fit_data`, which controls how many parallel processes are used to run the fits. This is set to 2 by default.
* Updated the article with the analysis of the Carpenter & Williams data to include details about the jittering.

# LATERmodel 0.1.0

* Initial version

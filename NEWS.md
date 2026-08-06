# dawnn 2.1.0 (5 August 2026)

Mainly changes with a view to submitting to CRAN.

* Model now hosted on Zenodo.
* `download_model()` verifies the size and MD5 checksum of a model downloaded
  from the default URL, deleting it and failing if either does not match.
* `download_model()` now saves the model to the user cache directory
  (`tools::R_user_dir("dawnn", "cache")`) by default. Other functions updated
  accordingly.
* `download_model()` reports an unreachable URL with an informative message,
  and no longer leaves the `timeout` option modified if a download fails.
* `run_dawnn()` no longer alters the global random number generator state.
* The default `verbosity` of `run_dawnn()` is now 1 rather than 2, so the
  progress output of the underlying `predict()` calls is supressed by default.
* Added more sanity checks.
* Vectorised p-value calculation.
* Model now downloaded in binary mode, fixing corrupted downloads on Windows.
* Added a vignette, a `CITATION` file, and package URLs.
* R (>= 4.0.0) is now required, up from R (>= 3.5.0).

# dawnn 2.0.0 (16 July 2026)

* Simultaneously test for local and global differential abundance.
* Only take single label from user (since two labels are assumed, the other
  need not be passed).

# dawnn 1.2.0 (15 July 2026)

* Fixed a bug where the `alpha` parameter was not being respected (the default
  value of 0.1 was always being used).

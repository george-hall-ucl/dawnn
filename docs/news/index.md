# Changelog

## dawnn 2.2.0 (7 September 2026)

CRAN release: 2026-09-07

This release contains two major bugfixes, both of which can change
results.

- Bugfix in p-value computation. Previously, we were accidentally
  computing one-tailed p-values but reporting them as two-tailed. Now,
  we actually compute two-tailed p-values, as claimed. This will likely
  alter results.
- Bugfix in Benjamini-Yekutieli procedure. It now steps up correctly,
  rather than stopping at the first p-value above its cutoff. This could
  alter results, but in our testing it does not seem to affect which
  cells Dawnn calls as in regions of DA whatsoever.

## dawnn 2.1.1 (21 August 2026)

CRAN release: 2026-09-01

- Documentation changes requested by CRAN: removed examples for
  unexported functions.

## dawnn 2.1.0 (5 August 2026)

Mainly changes with a view to submitting to CRAN.

- Model now hosted on Zenodo.
- [`download_model()`](https://george-hall-ucl.github.io/dawnn/reference/download_model.md)
  verifies the size and MD5 checksum of a model downloaded from the
  default URL, deleting it and failing if either does not match.
- [`download_model()`](https://george-hall-ucl.github.io/dawnn/reference/download_model.md)
  now saves the model to the user cache directory
  (`tools::R_user_dir("dawnn", "cache")`) by default. Other functions
  updated accordingly.
- [`download_model()`](https://george-hall-ucl.github.io/dawnn/reference/download_model.md)
  reports an unreachable URL with an informative message, and no longer
  leaves the `timeout` option modified if a download fails.
- [`run_dawnn()`](https://george-hall-ucl.github.io/dawnn/reference/run_dawnn.md)
  no longer alters the global random number generator state.
- The default `verbosity` of
  [`run_dawnn()`](https://george-hall-ucl.github.io/dawnn/reference/run_dawnn.md)
  is now 1 rather than 2, so the progress output of the underlying
  [`predict()`](https://rdrr.io/r/stats/predict.html) calls is
  suppressed by default.
- Added more sanity checks.
- Vectorised p-value calculation.
- Model now downloaded in binary mode, fixing corrupted downloads on
  Windows.
- Added a vignette, a `CITATION` file, and package URLs.
- R (\>= 4.0.0) is now required, up from R (\>= 3.5.0).

## dawnn 2.0.0 (16 July 2026)

- Simultaneously test for local and global differential abundance.
- Only take single label from user (since two labels are assumed, the
  other need not be passed).

## dawnn 1.2.0 (15 July 2026)

- Fixed a bug where the `alpha` parameter was not being respected (the
  default value of 0.1 was always being used).

# Download the neural network model used by Dawnn.

\`download_model()' downloads the neural network model used by Dawnn,
which is too large to be bundled with the package. This function must be
used once before run_dawnn() can be executed. After this, the path to
the model can be passed to this function.

## Usage

``` r
download_model(
  model_url = NULL,
  model_file_path = NULL,
  download_method = "auto",
  download_timeout = 600
)
```

## Arguments

- model_url:

  String url from which to download the model.

- model_file_path:

  String path at which to save the downloaded model.

- download_method:

  String download program to use (e.g. wget, curl etc).

- download_timeout:

  Integer number of seconds before download times out (optional, default
  = 600).

## Value

Message confirming the absolute path to the downloaded model.

## Examples

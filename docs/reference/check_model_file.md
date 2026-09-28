# Check a downloaded model against its expected size and checksum.

`check_model_file()` stops if the file at `model_file_path` is not the
model expected at Dawnn's default URL. The offending file is deleted, so
that a corrupt model cannot later be loaded by
[`run_dawnn()`](https://george-hall-ucl.github.io/dawnn/reference/run_dawnn.md).
The checksum is only compared when the size matches, so a truncated
download is reported once rather than twice.

## Usage

``` r
check_model_file(model_file_path, expected_size, expected_md5)
```

## Arguments

- model_file_path:

  String path to the downloaded model.

- expected_size:

  Integer expected size of the model, in bytes.

- expected_md5:

  String expected MD5 checksum of the model.

## Value

Invisibly, `TRUE` if the file matches, otherwise stop execution with an
error message.

# Browse Model Docs

Open a browser at the current version of a particular model in an online
`macpan2` model library.

## Usage

``` r
mp_model_docs(model_name, macpan_library = "starter_models")
```

## Arguments

- model_name:

  Name of a model in the `macpan_library`.

- macpan_library:

  Name of a library. Currently, the default value of
  `macpan_library = "starter_models"` is the only recommended option.

## Value

This function returns the URL of the library model, but the main purpose
is the side-effect of automatically opening a web browser at this URL.

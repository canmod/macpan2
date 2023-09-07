# Function to extract and execute R code from Rmd file
extract_and_source_r_code <- function(vignette_file) {
  # Create a temporary R script file (with .R extension)
  temp_script_file <- sub(".Rmd$", ".R", vignette_file)

  # Use knitr's purl to extract R code from the Rmd file
  knitr::purl(vignette_file, output = temp_script_file)

  # Run the extracted R script using source()
  message(sprintf("Now running code from %s", vignette_file))
  result = try(source(temp_script_file))

  # Clean up the temporary script file
  # file.remove(temp_script_file)

  if (inherits(result, "try-error")) {
    stop(sprintf("Failed to run code from %s", vignette_file))
  }
}

# Function to process all Rmd files in a directory
source_r_code_from_vignettes <- function(vignette_directory) {
  rmd_files <- list.files(
    vignette_directory,
    pattern = "\\.Rmd$",
    full.names = TRUE
  )
  for (vignette_file in rmd_files) {
    extract_and_source_r_code(vignette_file)
  }
}

source_r_code_from_vignettes("vignettes")

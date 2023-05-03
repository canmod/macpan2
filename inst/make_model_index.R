## what working directory are we starting from? for now, assume we're in inst/
model_dirs <- list.files(pattern="models$")

## this is already too big and ugly.
## could be simplify by tidyverse if we wanted
## maybe there's an existing pandoc-YAML-to-json-to-list pipeline?
get_mod_info <- function(f) {
    fields <- c("title", "index_entry")
    fn <- file.path(f, "README.md")
    if (!file.exists(fn)) return(NULL)
    txt <- readLines(fn)
    (lapply(as.list(fields),
           function(ff) {
               (grep(ff, x = txt, value = TRUE)
                   |> gsub(
                          pattern = sprintf("^[^:]+: *"),
                          replacement = "")
                   |> gsub(
                          pattern = "['\"]",
                          replacement = ""
                      )
               )
           }) 
        |> setNames(fields)
        |> as.data.frame()
    )
}

## get_mod_info("starter_models/sir/README.md")
    
               
model_list <- list()
for (d in model_dirs) {
    mods <- list.files(path = d)
    cat(mods, sep = "\n")
    model_list[[d]] <- do.call("rbind",
                               lapply(file.path(d, mods), get_mod_info))
}

## could embed this machinery in an rmd/qmd file, generate sections/tables


## this is already too big and ugly.
## could be simplify by tidyverse if we wanted
## maybe there's an existing pandoc-YAML-to-json-to-list pipeline?

## e.g. https://rdrr.io/cran/yaml/man/read_yaml.html
##  * readLines()
##  * |> extract header to variable (drop lines after second "^---"
##  * read_yaml(textConnection(header_contents))
## adds a (soft) dependency to yaml pkg
##  ... but no recursive dependencies ... https://github.com/vubiostat/r-yaml/blob/master/DESCRIPTION

## instead of hard-coding README.md, list.files with pattern
##  README.?md (specify [qQRr] ?) to allow dynamic files

get_mod_info <- function(f) {
    f_base <- basename(f)
    fields <- c("title", "index_entry")
    empty <- data.frame(rbind(rep(NA_character_, length(fields))))
    names(empty) <- fields
    fn <- list.files(path = f, pattern = "^README\\.[qr]?md$", ignore.case  = TRUE, full.names = TRUE)
    if (length(fn)==0) return(data.frame(dir = f_base, empty))
    if (length(fn)>1) stop("can't handle multiple README files")
    txt <- readLines(fn)
    res <- (lapply(as.list(fields),
                   function(ff) {
                       (grep(ff, x = txt, value = TRUE)
                           |> gsub(
                                  ## remove tag
                                  pattern = sprintf("^[^:]+: *"),
                                  replacement = "")
                           |> gsub(
                                  ## remove quotes at beginning or end of string
                                  pattern = "(^ *['\"]|['\"] *$)",
                                  replacement = ""
                              )
                       )
                   })
        |> setNames(fields)
        |> as.data.frame()
    )
    return(data.frame(dir=f_base, res))
}

#' Print a table of contents of available models
#'
#' Collects information from the headers of the README files in the model directories
#' and returns the results as a data frame
#'
#' @param dir directory to list
#' @param show_missing (logical) include entries for models with no README information?
#' @return a data frame containing entries \code{dir} (model directory), \code{title} (model title), \code{index_entry} (short description)
#' @examples show_models(show_missing = TRUE)
#' @importFrom stats na.omit
#' @export
show_models <- function(dir = system.file("starter_models", package = "macpan2"),
                        show_missing = FALSE) {
    mods <- list.files(path = dir)
    ## cat(mods, sep = "\n")
    res <- do.call("rbind",
                   lapply(file.path(dir, mods), get_mod_info))
    ord <- order(paste0(as.numeric(is.na(res$title)), res$dir))
    res <- res[ord,]
    if (show_missing) res else na.omit(res)
}

## could embed this machinery in an rmd/qmd file, generate sections/tables


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
    files <- list.files(path = f
      , pattern = "^README\\.[qr]?md$"
      , ignore.case  = TRUE
      , full.names = TRUE
    )
    rmdfiles <- grep("README\\.[qr]md$", files, ignore.case  = TRUE)
    if (length(rmdfiles) == 0L){
      fn <- files 
    } else {
      fn <- files[rmdfiles]
    }
    if (length(fn) == 0) return(data.frame(dir = f_base, empty))
    if (length(fn) > 1) {
      stop("There are too many readme files in this model library entry. Please contact the author of the model.")
    }
    txt <- readLines(fn)
    get_value = function(field, lines) {
      pattern = sprintf("^%s:\\s*", field)
      lines = grep(pattern, lines, value = TRUE)
      if (length(lines) == 0L) {
        stop("Cannot find the ", field, " in README header.")
      }
      value = sub(pattern, "", lines[1L])
      
      ## remove quotes at beginning or end of string
      value = gsub("(^ *['\"]|['\"] *$)", "", value)
      return(value)
    }
    
    res = (fields
      |> lapply(get_value, txt)
      |> setNames(fields)
      |> as.data.frame()
    )
    return(data.frame(dir=f_base, res))
}

#' Print a Table of Existing Models
#'
#' Collects information from the headers of the README files in
#' model directories and returns the results as a data frame
#'
#' @param dir directory to list
#' @param show_missing (logical) include entries for models with no README information?
#' @param for_markdown (logical) format for rendering the table with markdown-formatted
#' links to model readme files?
#' @return a data frame containing entries \code{Directory} (model directory), \code{Title} (model title), \code{Description} (short description)
#' @examples mp_show_models(show_missing = TRUE)
#' @importFrom stats na.omit
#' @export
mp_show_models <- function(
        dir = system.file("starter_models", package = "macpan2")
      , show_missing = FALSE
      , for_markdown = FALSE
    ) {
  
    ## TODO: handle multi-engine case when it is ready ...
    mods <- list.files(path = dir)
    ## ... but for now just list the models with a tmb.R file
    mods = (dir
      |> list.files("^tmb.R$", recursive = TRUE) 
      |> dirname()
    )
    
    ## cat(mods, sep = "\n")
    res <- do.call("rbind",
                   lapply(file.path(dir, mods), get_mod_info))
    ord <- order(paste0(as.numeric(is.na(res$title)), res$dir))
    res <- res[ord,]
    if (!show_missing) res <- na.omit(res)
    if (for_markdown) res$dir = model_link(res$dir)
    ## row numbering including missing models is distracting ...
    rownames(res) <- NULL
    
    res = setNames(res, c("Directory", "Title", "Description"))
    return(res)
}

#' @describeIn mp_show_models Synonym for `mp_show_models`, which is
#' preferred. Present for back-compatibility.
#' @export
show_models = mp_show_models

model_link = function(model_name) {
  sprintf(
      "[%s](https://github.com/canmod/macpan2/tree/main/inst/starter_models/%s)"
    , model_name, model_name
  )
}

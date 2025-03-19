# mp_wrap function is much better than anything below, which should be
# replaced. the usage is mp_wrap("this is a message") |> error().
# the error() can be replaced with warning() or message().
# https://stackoverflow.com/questions/45693010/how-do-you-format-multiline-r-package-messages
mp_wrap = function(...) c(...)  |> strwrap(prefix = "\n", initial = "")

msg <- function(..., .sep = "", .max_char_limit = getOption("width")) {
  input_string = (list(...)
    |> lapply(as.character)
    |> unlist(recursive = TRUE, use.names = FALSE)
    |> trimws(whitespace = "[ \t]")
    |> paste0(collapse = " ")
  )

  # Split the input string into words
  words <- unlist(strsplit(input_string, " "))

  # Initialize variables
  result <- character(0)
  current_word <- ""

  for (word in words) {
    # If adding the current word would exceed the character limit, start a new element
    if (nchar(paste(current_word, word, sep = " ")) > .max_char_limit) {
      result <- c(result, current_word)
      current_word <- ""
    }

    # Add the word to the current element
    current_word <- paste(current_word, word, sep = " ")
  }

  # Add the last element
  if (nchar(current_word) > 0) {
    result <- c(result, current_word)
  }

  break_start = function(x) sprintf("%s%s", .sep, x)
  (result
    |> trimws(whitespace = "[ \t]")
    |> break_start()
    |> paste0(collapse = "\n")
  )
}
msg_header = function(x, .max_char_limit = getOption("width")) {
  line = ("-"
   |> rep(.max_char_limit)
   |> paste0(collapse = "")
  )
  sprintf("\n%s\n%s\n%s\n", line, x, line)
}
msg_list = function(...) {
  (list(...)
   |> unlist()
   |> paste0(collapse = "\n")
  )
}
msg_hline = function(x = "", start_char = "\n", end_char = "\n", .max_char_limit = getOption("width")) {
  line = ("-"
   |> rep(.max_char_limit)
   |> paste0(collapse = "")
  )
  sprintf("%s%s%s%s", start_char, line, end_char, x)
}
msg_colon = function(x, y) sprintf("%s:\n%s", x, y)
msg_break = function(...) paste(paste(..., sep = "\n"), collapse = "\n")
msg_indent = function(..., .max_char_limit = getOption("width") - 4L) {
  (msg(..., .max_char_limit = .max_char_limit, .sep = "\n    ")
   |> trimws(which = "left", whitespace = "[\n]")
  )
}
msg_indent_break = function(...) {
  (list(...)
   |> lapply(sprintf, fmt = "    %s")
   |> unlist(use.names = FALSE, recursive = TRUE)
   |> msg_break()
  )
}
msg_paste = function(...) paste(..., sep = "")
msg_space = function(...) paste(..., sep = " ")
msg_csv   = function(...) paste(..., sep = ", ")

if (FALSE) {

word_vector <- c("In", "the", "midst", "of", "a", "bustling", "city", "where", "the", "cacophony", "of", "car", "horns", "the", "hustle", "of", "pedestrians", "rushing", "to", "their", "destinations", "and", "the", "towering", "skyscrapers", "that", "seemed", "to", "touch", "the", "heavens", "created", "a", "dynamic", "and", "overwhelming", "urban", "landscape", "a", "solitary", "tree", "stood", "as", "a", "silent", "sentinel", "its", "branches", "swaying", "gently", "in", "the", "breeze", "offering", "a", "tranquil", "oasis", "amidst", "the", "chaos", "a", "place", "where", "one", "could", "find", "solace", "and", "a", "moment", "of", "reflection", "in", "the", "heart", "of", "the", "metropolis")
msg_indent(word_vector) |> cat()

msg_colon(
  msg(
    "In the midst of a bustling city, where the cacophony of,",
    "the hustle of pedestrians rushing to their destinations, and the ",
    "towering skyscrapers that seemed to touch the heavens created a dynamic",
    "and overwhelming urban landscape",
    "a solitary tree stood as a silent sentinel",
    "its branches swaying gently in the breeze",
    "offering a tranquil oasis amidst the chaos",
    "a place where one could find solace and a moment of reflection in the",
    "heart of the metropolis"
  ),
  msg_indent(word_vector)
) |> cat()


expr_as_string = "y ~ x[i]"
missing_items = c("z", "a")
vec_type = "THINGS"
pointers = "this is nother thing"
vec = c(letters, letters, letters)
msg_break(
  msg_colon(
    "The expression given by",
    msg_indent(expr_as_string)
  ),
  msg_colon(
    msg("contained the following", vec_type),
    msg_indent(missing_items)
  ),
  msg_colon(
    msg("that were not found in the list of available", vec_type),
    msg_indent(vec)
  ),
  msg(pointers)
)|>cat()

}

% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/name_utils.R
\name{names_and_labels}
\alias{names_and_labels}
\alias{to_labels}
\alias{to_names}
\alias{to_name}
\alias{to_name_pairs}
\alias{to_values}
\title{Names and Labels}
\usage{
to_labels(x)

to_names(x)

to_name(x)

to_name_pairs(x)

to_values(x)
}
\arguments{
\item{x}{Object from which to extract its name, names, labels, name-pairs,
or values. Not all types of objects will work for all functions.}
}
\value{
Character vector (or numeric vector in the case of \code{to_values})
that describes \code{x}.
}
\description{
This page describes functions for giving names and labels to
entities in structured models.
}
\section{Functions}{
\itemize{
\item \code{to_labels()}: Extract a vector for describing the rows
of a data frame or values of a numeric vector.

\item \code{to_names()}: Extract a character vector for describing the
character-valued columns in a data frame or the flattened structure of
a numeric vector.
Names obey the following restrictions:  (1) they cannot have dots, (2) all
values must start with a letter, (3) all characters must be letters,
numbers, or underscore.

\item \code{to_name()}: Extract a string (i.e. length-1 character
vector) for describing the character-valued columns in a data frame or the
flattened structure of a numeric vector. The name of an object is the
dot-concatenation of its names.

\item \code{to_name_pairs()}: A character
vector with all possible pairwise dot-concatenations of a set of names.

\item \code{to_values()}: Extract the \code{\link{numeric}} column from a
data frame with only a single numerical column. This data frame might have
more than one column, but only one of them can be numeric. This function
will also turn numeric \code{\link{matrix}} and \code{\link{array}} objects
with \code{\link{dimnames}} into a flattened numeric vector with labels
produced by appropriately dot-concatenating the \code{\link{dimnames}}.

}}
\section{Context}{
A goal of \code{macpan2} is to provide a mechanism for representing structured
compartmental models. An example of such a model is to have each compartment
in an SEIR model split into a set of spatial locations and into a set of
age groups. It is crucial but difficult to assign meaningful and consistent
names to the compartments, flow rates, transmission rates, contact rates,
sub-population sizes, and other parameters determining these quantities.
Such names should convey how the different quantities relate to one another.
For example, the names should make clear that the rate of flow between two
compartments is specific to, say, the age group and location of those
compartments. The naming system should facilitate identifying model
quantities and sets of quantities. For example, in a spatially structured
model we might want to refer to all states in a particular location
(e.g. Toronto) and a specific state within that location (e.g. susceptible
individuals in Toronto).

Model entities (e.g. states, flow rates, transmission rates), can be
described using a data frame of string-valued columns. The rows of these data
frames represent the entities being represented.  The columns of the data
frame represent different ways to describe the rows.

\if{html}{\out{<div class="sourceCode r">}}\preformatted{EpiSympVax = data.frame(
  Epi = c(rep(c("S", "E", "I", "I", "R", "beta"), 2), "alpha", "gamma", "gamma", "infectiousness", "infectiousness", ""),
  Symp = c(rep(c("", "", "mild", "severe", "", ""), 2), "", "mild", "severe", "mild", "severe", ""),
  Vax = c(rep(c("unvax", "vax"), each = 6), "", "", "", "", "", "dose_rate")
)
EpiSympVax
#>               Epi   Symp       Vax
#> 1               S            unvax
#> 2               E            unvax
#> 3               I   mild     unvax
#> 4               I severe     unvax
#> 5               R            unvax
#> 6            beta            unvax
#> 7               S              vax
#> 8               E              vax
#> 9               I   mild       vax
#> 10              I severe       vax
#> 11              R              vax
#> 12           beta              vax
#> 13          alpha                 
#> 14          gamma   mild          
#> 15          gamma severe          
#> 16 infectiousness   mild          
#> 17 infectiousness severe          
#> 18                       dose_rate
}\if{html}{\out{</div>}}

Non-empty values in each cell must contain only letters, numbers, underscores,
and must start with a letter. Empty values are zero-length strings that can be
used to indicate that some partitions are not applicable to some variables. The
purpose for these restrictions is to facilitate the construction of strings and
character vectors that summarize different aspects of the data frame.
When taken together, these summaries can be inverted to restore the full
labelled partition and so they represent zero information loss. This equivalence
allows us to go back-and-forth between the two representations without loosing
information, but perhaps gaining convenience.

There are three types of summaries: the names, the name, and the labels. The
names of a data frame are the names of the string-valued columns.

\if{html}{\out{<div class="sourceCode r">}}\preformatted{to_names(EpiSympVax)
#> [1] "Epi"  "Symp" "Vax"
}\if{html}{\out{</div>}}

The name of a data frame is the dot-concatenation of the names.

\if{html}{\out{<div class="sourceCode r">}}\preformatted{to_name(EpiSympVax)
#> [1] "Epi.Symp.Vax"
}\if{html}{\out{</div>}}

The labels of a data frame is the row-wise dot-concatenation of the
string-valued columns.

\if{html}{\out{<div class="sourceCode r">}}\preformatted{to_labels(EpiSympVax)
#>  [1] "S..unvax"               "E..unvax"               "I.mild.unvax"          
#>  [4] "I.severe.unvax"         "R..unvax"               "beta..unvax"           
#>  [7] "S..vax"                 "E..vax"                 "I.mild.vax"            
#> [10] "I.severe.vax"           "R..vax"                 "beta..vax"             
#> [13] "alpha.."                "gamma.mild."            "gamma.severe."         
#> [16] "infectiousness.mild."   "infectiousness.severe." "..dose_rate"
}\if{html}{\out{</div>}}

These labels give a unique single character string for referring to each
variable. With only the labels and one of either the names or the name, one may
recover the labelled partition. The labels provide convenient names for the
variables -- i.e. rownames.
By convention we use \href{https://en.wikipedia.org/wiki/Camel_case}{UpperCamelCase}
for partition names and a modified form of
\href{https://en.wikipedia.org/wiki/Snake_case}{snake_case} for variable labels. Our
modification of snake case allows for single uppercase letters in order to
accommodate the convention in epidemiology for using single uppercase letters to
refer to state variables. For example, \code{S}, \code{I}, and \code{R}, as well as \code{I_mild}
and \code{I_severe}, would be consistent with our modified snake case style.
}


% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/trees_create_list_levels.R
\name{trees_create_list_levels}
\alias{trees_create_list_levels}
\title{Transform a tree tibble into a tree JSON}
\usage{
trees_create_list_levels(x, depth, level = 1)
}
\arguments{
\item{x}{Tibble. Classification tree for documents.}

\item{depth}{Integer.}

\item{level}{Integer.}
}
\value{
List. Multi-layered hierarchical classification equivalent to a json file structure
}
\description{
Recursive function used in trees_tibble_to_json to transform a list of documents into a json hierarchy using the position argument to organize the hierarchy (remove custom tags in the process)
}
\seealso{
trees_tibble_to_json

trees_fill_list_level
}
\author{
Nicolas Mangin
}

% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/tags_change_label.R
\name{tags_change_label}
\alias{tags_change_label}
\title{Change the label of a tag}
\usage{
tags_change_label(course_paths, old_label, new_label)
}
\arguments{
\item{course_paths}{Reactive. Function containing a list of paths to the different folders and databases on local disk.}

\item{old_label}{Character. Initial label to be replaced.}

\item{new_label}{Character. New label replacing the previous one}
}
\value{
Apply tag label change in the tag list and in all the documents.
}
\description{
Function changing the label of a tag in all documents.
}
\author{
Nicolas Mangin
}

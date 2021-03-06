\name{grouped_df}
\alias{grouped_df}
\alias{is.grouped_df}
\title{A grouped data frame.}
\usage{
grouped_df(data, vars, drop = TRUE)

is.grouped_df(x)
}
\arguments{
  \item{data}{a tbl or data frame.}

  \item{vars}{a list of quoted variables.}

  \item{drop}{if \code{TRUE} preserve all factor levels,
  even those without data.}
}
\description{
The easiest way to create a grouped data frame is to call
the \code{group_by} method on a data frame or tbl: this
will take care of capturing the unevalated expressions for
you.
}
\keyword{internal}


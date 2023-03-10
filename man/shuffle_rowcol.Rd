% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/data_management_utils.R
\name{shuffle_rowcol}
\alias{shuffle_rowcol}
\title{Shuffles placement grid by row and column
Note that this does not break the dependence of a panel on the row or column
i.e. two panels that originally share a row still share a row, and two panels
that originally share a column still share a column}
\usage{
shuffle_rowcol(grid)
}
\arguments{
\item{grid}{tibble with ROW, COL, and panel}
}
\value{
grid with rows and columns permuted from input grid
}
\description{
Shuffles placement grid by row and column
Note that this does not break the dependence of a panel on the row or column
i.e. two panels that originally share a row still share a row, and two panels
that originally share a column still share a column
}

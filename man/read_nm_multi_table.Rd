% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/read_nmtables.R
\name{read_nm_multi_table}
\alias{read_nm_multi_table}
\title{Read (single or) multiple NONMEM tables from a single file}
\usage{
read_nm_multi_table(
  fileName,
  header = TRUE,
  ...,
  simplify = TRUE,
  table_start_pattern = "^TABLE NO"
)
}
\arguments{
\item{fileName}{The filename to read from}

\item{header, ...}{Arguments passed to read.table}

\item{simplify}{If a single table is present, return a data.frame instead of
a list of data.frames?}

\item{table_start_pattern}{What should be found to start a new table?}
}
\value{
A list of data.frames, or if only one is present and simplify=TRUE, a
  data.frame.
}
\description{
Read (single or) multiple NONMEM tables from a single file
}
\examples{
\dontrun{
read_nm_multi_table("run1.cov", row.names=1)
}
}
\seealso{
Other NONMEM reading: 
\code{\link{plot_scm}()},
\code{\link{read_nm_all}()},
\code{\link{read_nmcov}()},
\code{\link{read_nmext}()},
\code{\link{read_nmtables}()},
\code{\link{read_nm}()},
\code{\link{read_scm}()}
}
\author{
Bill Denney
}
\concept{NONMEM reading}

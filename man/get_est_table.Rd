% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get_est_table.R
\name{get_est_table}
\alias{get_est_table}
\title{Create a table of model parameter estimates from a NONMEM output object.}
\usage{
get_est_table(
  x,
  thetaLabels = c(),
  omegaLabels = c(),
  sigmaLabels = c(),
  sigdig = 3
)
}
\arguments{
\item{x}{A NONMEM output object generated using \code{\link{read_nm}}.}

\item{thetaLabels}{A vector containing labels for THETA parameters.}

\item{omegaLabels}{A vector containing labels for OMEGA parameters.}

\item{sigmaLabels}{A vector containing labels for SIGMA parameters.}

\item{sigdig}{The desired number of significant digits to display.}
}
\value{
A named vector of NONMEM model parameter estimates.
}
\description{
Create a table of model parameter estimates from a NONMEM output object.
}
\examples{
\dontrun{
 nmOutput <- read_nm("run315.xml")
 estTab   <- get_est_table(nmOutput)
}

}
\seealso{
NONMEM (\url{https://www.iconplc.com/innovation/nonmem/})
}
\author{
Justin Wilkins, \email{justin.wilkins@occams.com}
}

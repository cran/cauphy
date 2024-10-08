% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/optimization.R
\name{fitCauchy.internal}
\alias{fitCauchy.internal}
\title{Maximum Likelihood estimator for a Cauchy model}
\usage{
fitCauchy.internal(
  phy,
  X,
  y,
  model = c("cauchy", "lambda"),
  method = c("reml", "random.root", "fixed.root"),
  starting.value = list(x0 = NULL, disp = NULL, lambda = NULL),
  lower.bound = list(disp = 0, lambda = 0),
  upper.bound = list(disp = Inf, lambda = 1),
  root.edge = 100,
  optim = c("local", "global"),
  method.init.disp = "Qn",
  ...
)
}
\arguments{
\item{phy}{a phylogenetic tree of class \code{\link[ape]{phylo}}.}

\item{model}{a model for the trait evolution. One of \code{"cauchy"} or \code{"lambda"} (see Details).}

\item{method}{the method used to fit the process.
One of \code{reml} (the default), \code{fixed.root} or \code{random.root}.
See Details.}

\item{starting.value}{starting value for the parameters of the Cauchy.
This should be a named list, with \code{x0} and \code{disp} the root starting value and the dispersion parameter.
The default initial values are computed from standard statistics used on (independent) Cauchy variables, see Details.}

\item{lower.bound}{named list with lower bound values for the parameters. See Details for the default values.}

\item{upper.bound}{named list with upper bound values for the parameters. See Details for the default values.}

\item{root.edge}{multiplicative factor for the root dispersion, equal to the length of the root edge. Ignored if \code{method!=random.root}.}

\item{optim}{if "local", only a local optimization around the initial parameter values is performed (the default).
If "global", a global maximization is attempted using the "MLSL" approach (see \code{\link[nloptr]{nloptr}}).}

\item{method.init.disp}{the initialization method for the dispersion.
One of "Qn", "Sn", "MAD", "IQR". Default to the "Qn" statistics.
See Details.}
}
\value{
A list, with the maximum likelihood rate parameter, and the likelihood value.
}
\description{
Find the maximum likelihood, using numerical optimization with \code{\link{optim}}.
}
\references{
Rothenberg T. J., Fisher F. M., Tilanus C. B. 1964. A Note on Estimation from a Cauchy Sample. Journal of the American Statistical Association. 59:460–463.
}
\seealso{
cauphylm
}
\keyword{internal}

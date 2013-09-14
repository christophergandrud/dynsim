\name{dynsim}
\alias{dynsim}
\title{Dynamically simulations of autoregressive relationships}
\usage{
  dynsim(ldv, scen1, scen24, modify = NULL, inter = NULL)
}
\arguments{
  \item{ldv}{character. Names the lagged dependent
  variable}

  \item{scen24}{THINK ABOUT}

  \item{sig}{numeric. Specifies the level of statistical
  significance of the confidence intervals.}

  \item{modify}{character vector. The name of up to four
  variables that modify the relationship between the shock
  vari- able and the dependent variable.}

  \item{inter}{character vector. The name of up to four
  interaction variables. If \code{modify} is specified,
  this must also be specified.}
}
\description{
  \code{dynsim} dynamic simulations of autoregressive
  relationships
}
\references{
  Williams, L. K., & Whitten, G. D. (2011). Dynamic
  Simulations of Autoregressive Relationships. The Stata
  Journal, 11(4), 577-588.

  Williams, L. K., & Whitten, G. D. (2012). But Wait,
  There’s More! Maximizing Substantive Inferences from TSCS
  Models. Journal of International Money and Finance,
  74(03), 685-693.
}


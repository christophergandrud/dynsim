\name{dynsim}
\alias{dynsim}
\title{Dynamically simulations of autoregressive relationships}
\usage{
  dynsim(ldv, scen1, scen2, scen3, scen4, n = 10, shock,
    shock_num, modify = NULL, inter = NULL)
}
\arguments{
  \item{ldv}{character. Names the lagged dependent
  variable}

  \item{scen1}{vector. Specifies the values of the
  variables used to generate the predicted values when
  \eqn{t = 0}.}

  \item{scen24}{THINK ABOUT}

  \item{n}{numeric. Specifies the number of iterations (or
  time period) over which the program will generate the
  predicted value of the dependent variable. The default is
  10.}

  \item{sig}{numeric. Specifies the level of statistical
  significance of the confidence intervals.}

  \item{shock}{character. Allows the user to choose an
  independent variable (and its first \code{n} values) and
  have the variable (and potentially different values)
  impact the scenarios at each simulation. The number of
  values assigned to the shock variable must exceed the
  number of simulations. If this command is specified, the
  user must specify the \code{n} shock values with
  \code{shock_num}. If the shock variable is interacted
  with another variable in the model, the user must also
  specify the name of the modifying variable
  (\code{modify}) and the interaction variable
  (\code{inter}).}

  \item{shock_num}{numeric vector. Specify the shock values
  Any numeric vector is acceptable, as long as it contains
  at least \code{n} values.}

  \item{modify}{character vector. The name of up to four
  variables that modify the relationship between the shock
  variable and the dependent variable.}

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


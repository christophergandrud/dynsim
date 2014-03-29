\name{dynsim}
\alias{dynsim}
\title{Dynamic simulations of autoregressive relationships}
\usage{
dynsim(obj, ldv, scen, n = 10, sig = 0.95, num = 1000, shocks = NULL,
  forecast = NULL)
}
\arguments{
  \item{obj}{the output object from \code{\link{zelig}}.}

  \item{ldv}{character. Names the lagged dependent
  variable}

  \item{scen}{data frame or list of data frames. Specifies
  the values of the variables used to generate the
  predicted values when \eqn{t = 0}. If only one scenario
  is desired then \code{scen} should be a data frame. If
  more than one scenario is desired then the \eqn{t = 0}
  values should be in data frames contained in a list.}

  \item{n}{numeric. Specifies the number of iterations (or
  time period) over which the program will generate the
  predicted value of the dependent variable. The default is
  10.}

  \item{sig}{numeric. Specifies the level of statistical
  significance of the confidence intervals. Any value
  allowed be greater than 0 and cannot be greater than 1.}

  \item{num}{numeric. Specifies the number of simulations
  to compute for each value of \code{n}. The default is
  1000.}

  \item{shocks}{data frame. Allows the user to choose
  independent variables, their values, and times to
  introduce these values. The first column of the data
  frame must be called \code{times} this will contain the
  times in \code{n} to use the shock values. The following
  columns' names must match the names of the variables
  whose values you wish to alter. You do not need to
  specify values for variables that you want to remain the
  same as in \code{scen}. In times \code{n} where shock
  values are not specified, non-\code{ldv} variable values
  will revert to those in \code{scen}. If \code{*} is used
  to create interactions, interaction terms will be fitted
  appropriately.}

  \item{forecast}{Reserved argument for future version. Any
  value given to \code{forecast} will be ignored.}
}
\value{
The command returns a \code{dynsim} class object. This can
contain up to seven elements: \itemize{
\item{\code{scenNumber}: }{The scenario number.}
\item{\code{time}: }{The time points.} \item{\code{shock.}:
}{Columns containing the values of the shock variables at
each point in \code{time}.} \item{\code{ldvMean}: }{Mean of
the simulation distribution.} \item{\code{ldvLower}:
}{Lower bound of the simulation distribution's central
interval set with \code{sig}.} \item{\code{ldvUpper}:
}{Upper bound of the simulation distribution's central
interval set with \code{sig}.} \item{\code{ldvLower50}:
}{Lower bound of the simulation distribution's central 50
percent interval.} \item{\code{ldvUpper50}: }{Upper bound
of the simulation distribution's central 50 percent
interval.} } You can easily convert the output object into
a standard data frame class object. For example, if the
object is called \code{dynOut} then use:
\code{class(dynOut) <- "data.frame"}.
}
\description{
\code{dynsim} dynamic simulations of autoregressive
relationships
}
\details{
A post-estimation technique for producing dynamic
simulations of autoregressive models estimated with
\code{\link{Zelig}}.
}
\examples{
# Load packages
library(Zelig)
library(DataCombine)

# Load Grunfeld data
data(grunfeld, package = "dynsim")

# Create lag invest variable
grunfeld <- slide(grunfeld, Var = "invest", GroupVar = "company",
               NewVar = "InvestLag")

# Estimate basic model
M1 <- zelig(invest ~ InvestLag + mvalue + kstock,
            model = "ls", data = grunfeld, cite = FALSE)

# Estimate model with interaction between mvalue and kstock
M2 <- zelig(invest ~ InvestLag + mvalue*kstock,
            model = "ls", data = grunfeld, cite = FALSE)

# Set up scenarios
attach(grunfeld)
Scen1 <- data.frame(InvestLag = mean(InvestLag, na.rm = TRUE),
                    mvalue = quantile(mvalue, 0.05),
                    kstock = quantile(kstock, 0.05))
Scen2 <- data.frame(InvestLag = mean(InvestLag, na.rm = TRUE),
                    mvalue = mean(mvalue),
                    kstock = mean(kstock))
Scen3 <- data.frame(InvestLag = mean(InvestLag, na.rm = TRUE),
                    mvalue = quantile(mvalue, 0.95),
                    kstock = quantile(kstock, 0.95))
detach(grunfeld)

# Combine into a single list
ScenComb <- list(Scen1, Scen2, Scen3)

## Run dynamic simulations without shocks and no interactions
Sim1 <- dynsim(obj = M1, ldv = "InvestLag", scen = ScenComb, n = 20)

## Run dynamic simulations without shocks and interactions
Sim2 <- dynsim(obj = M2, ldv = "InvestLag", scen = ScenComb, n = 20)

## Run dynamic simulations with shocks

# Create data frame of shock values
mShocks <- data.frame(times = c(5, 10), kstock = c(100, 1000),
                      mvalue = c(58, 5000))

# Run simulations without interactions
Sim3 <- dynsim(obj = M1, ldv = "InvestLag", scen = ScenComb, n = 20,
               shocks = mShocks)

# Run simulations with interactions
Sim4 <- dynsim(obj = M2, ldv = "InvestLag", scen = ScenComb, n = 20,
               shocks = mShocks)
}
\references{
Williams, L. K., & Whitten, G. D. (2011). Dynamic
Simulations of Autoregressive Relationships. The Stata
Journal, 11(4), 577-588.

Williams, L. K., & Whitten, G. D. (2012). But Wait, There's
More! Maximizing Substantive Inferences from TSCS Models.
Journal of Politics, 74(03), 685-693.
}


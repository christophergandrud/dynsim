\name{dynsimGG}
\alias{dynsimGG}
\title{Plot dynamic simulation results}
\usage{
  dynsimGG(obj, lsize = 1, color = NULL, alpha = 0.5,
    xlab = "\nTime", ylab = "Predicted Value\n",
    title = NULL, leg.name = "Scenario", leg.labels = NULL,
    legend = "legend")
}
\arguments{
  \item{obj}{a \code{Dynsim} class object.}

  \item{lsize}{size of the smoothing line. Default is 1.
  See \code{\link{ggplot2}}.}

  \item{color}{character string. Specifies the color of the
  lines and ribbons. If only one scenario is to be plotted
  then it can either be a single color value using any
  color value allowed by \code{\link{ggplot2}}. The default
  is the hexadecimal color \code{"#2B8CBE"}. If more than
  one scenario is to be plotted then a color brewer palette
  is set. The default is\code{"Set1"}. See
  \code{\link{scale_colour_brewer}}.}

  \item{alpha}{numeric. Alpha (e.g. transparency) for the
  points or ribbons. Default is \code{alpha = 0.1}. See
  \code{\link{ggplot2}}.}

  \item{xlab}{a label for the plot's x-axis.}

  \item{ylab}{a label of the plot's y-axis.}

  \item{title}{the plot's main title.}

  \item{leg.name}{name of the legend (if applicable).}

  \item{legend}{specifies what type of legend to include
  (if applicable). The default is \code{legend = "legend"}.
  To hide the legend use \code{legend = FALSE}. See
  \code{\link{discrete_scale}} for more details.}

  \item{leg.labels}{character vector specifying the labels
  for each scenario in the legend.}
}
\description{
  \code{dynsimGG} uses \code{\link{ggplot2}} to plot
  dynamic simulation results created by
  \code{\link{Dynsim}}.
}
\details{
  Plots dynamic simulations of autoregressive relationships
  from \code{\link{Dynsim}}. The central line is the mean
  of the simulation distributions. The outer ribbon is the
  furthest extent of the simulation distributions' central
  intervals found in \code{\link{Dynsim}} with the
  \code{sig} argument. The middle ribbons plot the limits
  of the simulation distributions' central 50% intervals.
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

# Set up a scenario
attach(grunfeld)
Scen1 <- data.frame(invest = 317, InvestLag = mean(InvestLag, na.rm = TRUE),
                    mvalue = quantile(mvalue, 0.05), kstock = quantile(kstock, 0.05))
Scen2 <- data.frame(invest = 100, InvestLag = mean(InvestLag, na.rm = TRUE),
                    mvalue = mean(mvalue), kstock = mean(kstock))
Scen3 <- data.frame(invest = 317, InvestLag = mean(InvestLag, na.rm = TRUE),
                    mvalue = quantile(mvalue, 0.95), kstock = quantile(kstock, 0.95))
detach(grunfeld)

# Combine into a single list
ScenComb <- list(Scen1, Scen2, Scen3)

## Run dynamic simulations without shocks
Sim1 <- Dynsim(obj = M1, ldv = "InvestLag", scen = ScenComb, n = 20)

# Create plot legend label
Labels <- c("5th Percentile", "Mean", "95th Percentile")

# Plot
dynsimGG(Sim1, leg.labels = Labels)

## Run dynamic simulations with shocks

# Create data frame of shock values
mShocks <- data.frame(times = c(5, 10), kstock = c(100, 1000))

# Run simulations
Sim2 <- Dynsim(obj = M1, ldv = "InvestLag", scen = ScenComb, n = 20,
               shocks = mShocks)

# Plot
dynsimGG(Sim2, leg.labels = Labels)
}


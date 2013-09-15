\name{dynsimGG}
\alias{dynsimGG}
\title{Plot dynamic simulation results}
\usage{
  dynsimGG(obj, lsize = 1, color = NULL, alpha = 0.5,
    xlab = "\nTime", ylab = "Predicted Value\n",
    title = NULL, leg.name = "Scenario", legend = "legend")
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

  \item{xlab}{a label for the plot's x-axis.}

  \item{ylab}{a label of the plot's y-axis.}

  \item{title}{the plot's main title.}

  \item{leg.name}{name of the legend (if applicable).}

  \item{legend}{specifies what type of legend to include
  (if applicable). The default is \code{legend = "legend"}.
  To hide the legend use \code{legend = FALSE}. See
  \code{\link{discrete_scale}} for more details.}
}
\description{
  \code{dynsimGG} uses \code{\link{ggplot2}} to plot
  dynamic simulation results created by
  \code{\link{dynsim}}.
}
\details{
  Plots dynamic simulations of autoregressive relationships
  from \code{\link{dynsim}}. The central line is the mean
  of the simulation distributions. The outer ribbon is the
  furthest extent of the simulation distributions' central
  intervals found in \code{\link{dynsim}} with the
  \code{sig} argument. The middle ribbons plot the limits
  of the simulation distributions' central 50% intervals.
}


\name{dynsimGG}
\alias{dynsimGG}
\title{Plot dynamic simulation results}
\usage{
  dynsimGG(obj, lsize = 1, color = NULL, alpha = 0.5,
    xlab = "\nTime", ylab = "Predicted Value\n",
    title = NULL, leg.name = "Scenario", legend = "legend")
}
\arguments{
  \item{obj}{a \code{dynsim} class object.}
}
\description{
  \code{dynsimGG} uses \code{\link{ggplot2}} to plot
  dynamic simulation results created by
  \code{\link{dynsim}}.
}


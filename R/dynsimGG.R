#' Plot dynamic simulation results
#'
#' \code{dynsimGG} uses \code{\link{ggplot2}} to plot dynamic simulation results created by \code{\link{dynsim}}.
#'
#' @param obj a \code{Dynsim} class object.
#' @param lsize size of the smoothing line. Default is 1. See \code{\link{ggplot2}}.
#' @param color character string. Specifies the color of the lines and ribbons. If only one scenario is to be plotted then it can either be a single color value using any color value allowed by \code{\link{ggplot2}}. The default is the hexadecimal color \code{"#2B8CBE"}. If more than one scenario is to be plotted then a color brewer palette is set. The default is\code{"Set1"}. See \code{\link{scale_colour_brewer}}.
#' @param xlab a label for the plot's x-axis.
#' @param ylab a label of the plot's y-axis.
#' @param title the plot's main title.
#' @param leg.name name of the legend (if applicable).
#' @param legend specifies what type of legend to include (if applicable). The default is \code{legend = "legend"}. To hide the legend use \code{legend = FALSE}. See \code{\link{discrete_scale}} for more details.
#'
#' @details Plots dynamic simulations of autoregressive relationships from \code{\link{dynsim}}. The central line is the mean of the simulation distributions. The outer ribbon is the furthest extent of the simulation distributions' central intervals found in \code{\link{dynsim}} with the \code{sig} argument. The middle ribbons plot the limits of the simulation distributions' central 50% intervals.
#'
#'
#' @import ggplot2
#'
#' @export
dynsimGG <- function(obj, lsize = 1, color = NULL, alpha = 0.5, xlab = "\nTime", ylab = "Predicted Value\n", title = NULL, leg.name = "Scenario", legend = "legend"){
	# Check if obj is of the dynsim class
	if (class(obj) != "Dynsim"){
		stop("obj must be a dynsim class object.")
	}
	# Reclass obj as a data frame for ggplot2
	class(obj) <- "data.frame"

	# Plot for one scenario
	if (!isTRUE("scenNumber" %in% names(obj))){
		if (is.null(color)){
			color <- "#2B8CBE" 
		}	
		ggplot(obj, aes(time, ldvMean)) +
			geom_line(size = lsize, colour = color) +
			geom_ribbon(aes(ymin = ldvLower, ymax = ldvUpper), alpha = alpha, fill = color, linetype = 0) +
			geom_ribbon(aes(ymin = ldvLower50, ymax = ldvUpper50), alpha = alpha, fill = color, linetype = 0) +
	        xlab(xlab) + ylab(ylab) +
	        ggtitle(title) +
	        theme_bw(base_size = 15)
	}
	# Plot multiple scenarios
	else if (isTRUE("scenNumber" %in% names(obj))){
		if (is.null(color)){
			color <- "Set1" 
		}	
		ggplot(obj, aes(time, ldvMean, colour = factor(scenNumber), fill = factor(scenNumber))) +
			geom_line(size = lsize) +
			geom_ribbon(aes(ymin = ldvLower, ymax = ldvUpper), alpha = alpha, linetype = 0) +
			geom_ribbon(aes(ymin = ldvLower50, ymax = ldvUpper50), alpha = alpha, linetype = 0) +
	        scale_colour_brewer(palette = color, name = leg.name, guide = legend) +
	        scale_fill_brewer(palette = color, name = leg.name, guide = legend) +
	        xlab(xlab) + ylab(ylab) +
	        ggtitle(title) +
	        theme_bw(base_size = 15)
	}
}
#' Plot dynamic simulation results
#'
#' \code{dynsimGG} uses \code{\link{ggplot2}} to plot dynamic simulation results created by \code{\link{dynsim}}.
#'
#' @param obj a \code{dynsim} class object.
#'
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
	        scale_colour_brewer(palette = color, name = leg.name, guide = legend) +
	        scale_fill_brewer(palette = color, name = leg.name, guide = legend) +
	        xlab(xlab) + ylab(ylab) +
	        ggtitle(title) +
	        theme_bw(base_size = 15)
	}
}
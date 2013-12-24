#' Plot dynamic simulation results from \code{dynsim}
#'
#' \code{dynsimGG} uses \code{\link{ggplot2}} to plot dynamic simulation results created by \code{\link{dynsim}}.
#'
#' @param obj a \code{dynsim} class object.
#' @param lsize size of the smoothing line. Default is 1. See \code{\link{ggplot2}}.
#' @param color character string. Specifies the color of the lines and ribbons. If only one scenario is to be plotted then it can either be a single color value using any color value allowed by \code{\link{ggplot2}}. The default is the hexadecimal color \code{"#2B8CBE"}. If more than one scenario is to be plotted then a color brewer palette is set. The default is\code{"Set1"}. See \code{\link{scale_colour_brewer}}.
#' @param alpha numeric. Alpha (e.g. transparency) for the ribbons. Default is \code{alpha = 0.1}. See \code{\link{ggplot2}}.
#' @param xlab a label for the plot's x-axis.
#' @param ylab a label of the plot's y-axis.
#' @param title the plot's main title.
#' @param leg.name name of the legend (if applicable).
#' @param legend specifies what type of legend to include (if applicable). The default is \code{legend = "legend"}. To hide the legend use \code{legend = FALSE}. See \code{\link{discrete_scale}} for more details.
#' @param leg.labels character vector specifying the labels for each scenario in the legend. 
#' @param shockplot.var character string naming the one shock variable to plot fitted values of over time specified underneath the main plot. 
#' @param shockplot.ylab a label for the shockplot's y-axis.
#'
#' @details Plots dynamic simulations of autoregressive relationships from \code{\link{dynsim}}. The central line is the mean of the simulation distributions. The outer ribbon is the furthest extent of the simulation distributions' central intervals found in \code{\link{dynsim}} with the \code{sig} argument. The middle ribbons plot the limits of the simulation distributions' central 50% intervals.
#'
#' @examples
#' # Load packages
#' library(Zelig)
#' library(DataCombine)
#' 
#' # Load Grunfeld data
#' data(grunfeld, package = "dynsim")
#' 
#' # Create lag invest variable
#' grunfeld <- slide(grunfeld, Var = "invest", GroupVar = "company", 
#'                NewVar = "InvestLag")
#' 
#' # Estimate basic model 
#' M1 <- zelig(invest ~ InvestLag + mvalue + kstock, 
#'             model = "ls", data = grunfeld, cite = FALSE)
#'             
#' # Set up scenarios
#' attach(grunfeld) 
#' Scen1 <- data.frame(InvestLag = mean(InvestLag, na.rm = TRUE), 
#'                     mvalue = quantile(mvalue, 0.05), kstock = quantile(kstock, 0.05))
#' Scen2 <- data.frame(InvestLag = mean(InvestLag, na.rm = TRUE), 
#'                     mvalue = mean(mvalue), kstock = mean(kstock))
#' Scen3 <- data.frame(InvestLag = mean(InvestLag, na.rm = TRUE), 
#'                     mvalue = quantile(mvalue, 0.95), kstock = quantile(kstock, 0.95))
#' detach(grunfeld)
#'                     
#' # Combine into a single list
#' ScenComb <- list(Scen1, Scen2, Scen3)
#' 
#' ## Run dynamic simulations without shocks
#' Sim1 <- dynsim(obj = M1, ldv = "InvestLag", scen = ScenComb, n = 20)
#' 
#' # Create plot legend label
#' Labels <- c("5th Percentile", "Mean", "95th Percentile")
#' 
#' # Plot
#' dynsimGG(Sim1, leg.labels = Labels)
#' 
#' ## Run dynamic simulations with shocks
#' 
#' # Create data frame of shock values
#' mShocks <- data.frame(times = c(5, 10), kstock = c(100, 1000))
#' 
#' # Run simulations
#' Sim2 <- dynsim(obj = M1, ldv = "InvestLag", scen = ScenComb, n = 20,
#'                shocks = mShocks)
#'                
#' # Plot
#' dynsimGG(Sim2, leg.labels = Labels)
#'
#' # Plot with accompanying shock plot
#' dynsimGG(Sim2, leg.labels = Labels, shockplot.var = "kstock")
#'
#' @import ggplot2
#' @importFrom gridExtra grid.arrange
#' @importFrom gridExtra arrangeGrob
#'
#' @export

dynsimGG <- function(obj, lsize = 1, color = NULL, alpha = 0.5, xlab = "\nTime", ylab = "Predicted Value\n", title = NULL, leg.name = "Scenario", leg.labels = NULL, legend = "legend", shockplot.var = NULL, shockplot.ylab = NULL){
	# CRAN requirements
	ldvMean <- ldvLower <- ldvUpper <- ldvLower50 <- ldvUpper50 <- scenNumber <- shockvar <- NULL

	# Check if obj is of the dynsim class
	if (class(obj) != "dynsim"){
		stop("obj must be a dynsim class object.")
	}
	# Reclass obj as a data frame for ggplot2
	class(obj) <- "data.frame"
	# Create legend values if none are specified
	if (is.null(leg.labels)){
		leg.labels <- as.character(unique(obj$scenNumber))
	}

	# Plot for one scenario
	if (!isTRUE("scenNumber" %in% names(obj))){
		if (is.null(color)){
			color <- "#2B8CBE" 
		}	
		MainPlot <- ggplot(obj, aes(time, ldvMean)) +
					geom_line(size = lsize, colour = color) +
					geom_ribbon(aes(ymin = ldvLower, ymax = ldvUpper), alpha = alpha, fill = color, linetype = 0) +
					geom_ribbon(aes(ymin = ldvLower50, ymax = ldvUpper50), alpha = alpha, fill = color, linetype = 0) +
			        xlab(xlab) + ylab(ylab) +
			        ggtitle(title) +
			        theme_bw(base_size = 15)
		# Add shock fitted value plot
		if (!is.null(shockplot.var)){
		  if (length(shockplot.var) > 1){
		    stop("You must specify ONE shock variable to plot with the shockplot.var argument.")
		  }
		  if (is.null(shockplot.ylab)){
		  	shockplot.ylab <- paste0(shockplot.var, "\n")
		  }
		  shockvar.pos <- paste0("shock.", shockplot.var)
		  shockvar.pos <- match(shockvar.pos, names(obj))
		  if(is.null(shockvar.pos)){
		    stop(paste(shockplot.var, "was not used as a shock variable."))
		  }
		  shockplot.df <- obj[, c(1, shockvar.pos)]
		  names(shockplot.df) <- c("time", "shockvar")
		  
		  ShockPlot <- ggplot(shockplot.df, aes(time, shockvar)) + 
		    geom_line(colour = color) +
		    ylab(shockplot.ylab) + xlab("") +
		    theme_bw(base_size = 10)
		  
		  gA <- ggplotGrob(MainPlot)
		  gB <- ggplotGrob(ShockPlot)
		  
		  # Set equal widths
		  maxWidth = grid::unit.pmax(gA$widths[2:5], gB$widths[2:5])
		  gA$widths[2:5] <- as.list(maxWidth)
		  gB$widths[2:5] <- as.list(maxWidth)
		  
		  grid.arrange(arrangeGrob(gA, gB, ncol = 1, heights = c(4, 1)))
		}
		else if (is.null(shockplot.var)){
		  MainPlot
		}
	}
	# Plot multiple scenarios
	else if (isTRUE("scenNumber" %in% names(obj))){
		if (is.null(color)){
			color <- "Set1" 
		}	
		MainPlot <- ggplot(obj, aes(time, ldvMean, colour = factor(scenNumber), fill = factor(scenNumber))) +
					geom_line(size = lsize) +
					geom_ribbon(aes(ymin = ldvLower, ymax = ldvUpper), alpha = alpha, linetype = 0) +
					geom_ribbon(aes(ymin = ldvLower50, ymax = ldvUpper50), alpha = alpha, linetype = 0) +
			        scale_colour_brewer(palette = color, name = leg.name, guide = legend, 
			        	labels = leg.labels) +
			        scale_fill_brewer(palette = color, name = leg.name, guide = legend, 
			        	labels = leg.labels) +
			        xlab(xlab) + ylab(ylab) +
			        ggtitle(title) +
			        theme_bw(base_size = 15)
	
    # Add shock fitted value plot
    if (!is.null(shockplot.var)){
      if (length(shockplot.var) > 1){
        stop("You must specify ONE shock variable to plot with the shockplot.var argument.")
      }
	  if (is.null(shockplot.ylab)){
	  	shockplot.ylab <- paste0(shockplot.var, "\n")
	  }
      shockvar.pos <- paste0("shock.", shockplot.var)
      shockvar.pos <- match(shockvar.pos, names(obj))
      if(is.null(shockvar.pos)){
        stop(paste(shockplot.var, "was not used as a shock variable."))
      } else
      shockplot.df <- obj[, c(1:2, shockvar.pos)]
      names(shockplot.df) <- c("scenNumber", "time", "shockvar")
      
      ShockPlot <- ggplot(shockplot.df, aes(time, shockvar, colour = as.factor(scenNumber))) + 
        geom_line() +
        scale_colour_brewer(palette = color, guide = FALSE) +
        ylab(shockplot.ylab) + xlab("") +
        theme_bw(base_size = 10)
      
      gA <- ggplotGrob(MainPlot)
      gB <- ggplotGrob(ShockPlot)
      
      # Set equal widths
      maxWidth = grid::unit.pmax(gA$widths[2:5], gB$widths[2:5])
      gA$widths[2:5] <- as.list(maxWidth)
      gB$widths[2:5] <- as.list(maxWidth)
      
    grid.arrange(arrangeGrob(gA, gB, ncol = 1, heights = c(4, 1)))
    }
		else if (is.null(shockplot.var)){
      MainPlot
    }
	}
}

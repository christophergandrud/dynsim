#' Dynamically simulate one scenario
#'
#' \code{OneScen} is an internal function to dynamically simulate one scenario.
#'
#' 
#' @importFrom Zelig setx
#' @importFrom Zelig sim
#' @importFrom Zelig simulation.matrix
#' @importFrom DataCombine MoveFront
#'
#' @keywords internals
#' @noRd

OneScen <- function(obj, ldv, n, scen, sig, shocks = NULL){
	# CRAN requirements
	times <- NULL
	
	# Create lower and upper bounds of the confidence interval
	Bottom <- (1 - sig)/2
	Top <- 1 - Bottom	

	# Create data frame to fill in with simulation summaries
	SimSum <- data.frame()
  ShockVals <- data.frame()

	# Change data frame for shock values
	for (i in 1:n){
		if (is.null(shocks)){
			scenTemp <- scen
		}
		else if (!is.null(shocks)){
			if (i %in% shocks[, "times"]){
			  scenTemp <- scen
				for (x in names(shocks)[-1]){
					shocksTemp <- subset(shocks, times == i)
					scenTemp[, x] <- shocksTemp[1, x]
				}
			}
			else if (!(i %in% shocks)){
				scenTemp <- scen
			}
		}

		# Run simulations
		SetVales <- setx(obj = obj, data = scenTemp)
		SimValues <- sim(obj = obj, x = SetVales)

		# Create summary data frame
		PV <- simulation.matrix(SimValues, "Predicted Values: Y|X")
		time <- i
		ldvMean <- mean(PV)
		ldvLower <- quantile(PV, prob = Bottom, names = FALSE)
		ldvUpper <- quantile(PV, prob = Top, names = FALSE)
		ldvLower50 <- quantile(PV, prob = 0.25, names = FALSE)
		ldvUpper50 <-quantile(PV, prob = 0.75, names = FALSE)
    
    # Shock variable values
	if (!is.null(shocks)){
		ShockNames <- names(shocks)[-1]
	    TempShock <- scenTemp[, ShockNames]
	    ShockVals <- rbind(ShockVals, TempShock)
    }
    # Combine
		TempOut <- cbind(time, ldvMean, ldvLower, ldvUpper, ldvLower50, ldvUpper50)
		SimSum <- rbind(SimSum, TempOut)

		# Change lag variable for the next simulation
		scen[, ldv] <- ldvMean		 
	}
  # Clean up shocks
	if (!is.null(shocks)){
		CleanNames <- paste0("shock.", ShockNames)
		names(ShockVals) <- CleanNames
		SimSum <- cbind(ShockVals, SimSum)
	}
	SimSum <- MoveFront(SimSum, "time")
	SimSum
}

#' Extract legend from ggplot2 object
#' 
#' @source Hadley Wickham
#' @import ggplot2
#' @keywords internals
#' @noRd 

gLegend <- function(Plot){
  tmp <- ggplot_gtable(ggplot_build(Plot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)
}
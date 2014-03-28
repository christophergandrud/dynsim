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

OneScen <- function(obj, ldv, n, scen, sig, num, shocks, forecast){
	# CRAN requirements
	times <- sigma.sqr <- alpha.sqr <- NULL
	
	# Create lower and upper percentile bounds of the confidence interval
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
    
		# Add in dependent variable for setx
		DV <- as.character(obj$formula[2])
		DVmean <- data.frame(mean(obj$data[, DV], na.rm = TRUE))
		names(DVmean) <- DV
		scenTemp <- cbind(DVmean, scenTemp)

		# Run simulations
		SetVales <- setx(obj = obj, data = scenTemp)
		SimValues <- sim(obj = obj, x = SetVales, num = num)

		# Create summary data frame
		PV <- simulation.matrix(SimValues, "Predicted Values: Y|X")
		time <- i
		ldvMean <- mean(PV)
    
    if (is.null(forecast)){
		  ldvLower <- quantile(PV, prob = Bottom, names = FALSE)
		  ldvUpper <- quantile(PV, prob = Top, names = FALSE)
		  ldvLower50 <- quantile(PV, prob = 0.25, names = FALSE)
		  ldvUpper50 <-quantile(PV, prob = 0.75, names = FALSE)
    }
    else if (!is.null(forecast)){
      sigma.sqrDF <- 
      alpha.sqrDF <- 
      if (forecast == "ag"){

        iMinusOne <- i - 1
        se <- sqrt(sigma.sqr *  (1 * iMinusOne * sigma.sqr * alpha.sqr)) 
        
      }
    }
    
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

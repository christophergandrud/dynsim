#' Dynamically simulate one scenario
#'
#' \code{OneScen} is an internal function to dynamically simulate one scenario.
#'
#' 
#' @importFrom Zelig setx
#' @importFrom Zelig sim
#' @importFrom Zelig simulation.matrix
#'
#' @keywords internals
#' @noRd

OneScen <- function(obj, ldv, n, scen, sig, shocks = NULL){
	# Create lower and upper bounds of the confidence interval
	Bottom <- (1 - sig)/2
	Top <- 1 - Bottom	

	# Create data frame to fill in with simulation summaries
	SimSum <- data.frame()

	# Change data frame for shock values
	for (i in 1:n){
		if (is.null(shocks)){
			scenTemp <- scen
		}
		else if (!is.null(shocks)){
			if (i %in% shocks[, "times"]){
				for (x in names(shocks)[-1]){
					scenTemp <- scen
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

		TempOut <- cbind(time, ldvMean, ldvLower, ldvUpper, ldvLower50, ldvUpper50)
		SimSum <- rbind(SimSum, TempOut)

		# Change lag variable for the next simulation
		scen[, ldv] <- ldvMean		 
	}
	SimSum
}
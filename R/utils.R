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

OneScen <- function(obj, ldv, n, scen, sig){
	# Create lower and upper bounds of the confidence interval
	Bottom <- (1 - sig)/2
	Top <- 1 - Bottom	

	# Create data frame to fill in with simulation summaries
	SimSum <- data.frame()

	for (i in 1:n){
		# Run simulations
		SetVales <- setx(obj = obj, data = scen)
		SimValues <- sim(obj = obj, x = SetVales)

		# Create summary data frame
		PV <- simulation.matrix(SimValues, "Predicted Values: Y|X")
		time <- i
		ldvMean <- mean(PV)
		ldvLower <- quantile(PV, prob = Bottom)
		ldvUpper <- quantile(PV, prob = Top)

		TempOut <- cbind(time, ldvMean, ldvLower, ldvUpper)
		SimSum <- rbind(SimSum, TempOut)

		# Change lag variable for the next simulation
		scen[, ldv] <- ldvMean		 
	}
	SimSum
}
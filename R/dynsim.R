#' Dynamically simulations of autoregressive relationships
#' 
#' \code{dynsim} dynamic simulations of autoregressive relationships
#' 
#' @obj the output object from \code{\link{zelig}}.
#' @param ldv character. Names the lagged dependent variable
#' @param scen vector. Specifies the values of the variables used to generate the predicted values when \eqn{t = 0}.
#' @param scen24 THINK ABOUT
#' @param n numeric. Specifies the number of iterations (or time period) over which the program will generate the predicted value of the dependent variable. The default is 10.
#' @param sig numeric. Specifies the level of statistical significance of the confidence intervals. Any value allowed be greater than 0 and cannot be greater than 1.
#'
#' @param shock character. Allows the user to choose an independent variable (and its first \code{n} values) and have the variable (and potentially different values) impact the scenarios at each simulation. The number of values assigned to the shock variable must exceed the number of simulations. If this command is specified, the user must specify the \code{n} shock values with \code{shock_num}. If the shock variable is interacted with another variable in the model, the user must also specify the name of the modifying variable (\code{modify}) and the interaction variable (\code{inter}).
#' @param shock_num numeric vector. Specify the shock values Any numeric vector is acceptable, as long as it contains at least \code{n} values.
#' @param modify character vector. The name of up to four variables that modify the relationship between the shock variable and the dependent variable. 
#' @param inter character vector. The name of up to four interaction variables. If \code{modify} is specified, this must also be specified.
#' 
#' 
#' 
#' 
#' @references 
#' Williams, L. K., & Whitten, G. D. (2011). Dynamic Simulations of Autoregressive Relationships. The Stata Journal, 11(4), 577-588.
#' 
#' Williams, L. K., & Whitten, G. D. (2012). But Wait, There’s More! Maximizing Substantive Inferences from TSCS Models. Journal of International Money and Finance, 74(03), 685-693.
#' 
#' @importFrom Zelig setx
#' @importFrom Zelig sim
#' @importFrom Zelig simulation.matrix
#'
#' @export

dynsim <- function(obj, ldv, scen, n = 10, sig = 0.95, shock, shock_num, modify = NULL, inter = NULL){
	# Make sure both modify and inter are given, if necessary.
	if (!is.null(modify) & is.null(inter)){
		stop("You must also specify the inter argument if you specify modify.")
	}
	if (is.null(modify) & !is.null(inter)){
		stop("You must also specify the modify argument if you specify inter.")
	}
	# Get the number of modify and inter variables; verify that the number is the same.
	if (length(modify) != length(inter)){
		stop("The number of modify variables must be the same as the number of inter variables.")
	}	
	# Error if number of iterations is <= 0.
	if (n <= 0){
		stop("You must specify at least 1 iteration with the n argument.")
	}
	# Make sure sig is between 0 and 1.
	if (sig <= 0 | sig > 1){
		stop("sig must be greater than 0 and not greater than 1.")
	}
	# Create lower and upper bounds of the confidence interval
	Bottom <- (1 - sig)/2
	Top <- 1 - Bottom	

	# Create data frame to fill in with simulation summaries
	SimSum <- data.frame()

	# Predicted values at time t0
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





}






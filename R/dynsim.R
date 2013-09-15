#' Dynamically simulations of autoregressive relationships
#' 
#' \code{dynsim} dynamic simulations of autoregressive relationships
#' 
#' @obj the output object from \code{\link{zelig}}.
#' @param ldv character. Names the lagged dependent variable
#' @param scen data frame or list of data frames. Specifies the values of the variables used to generate the predicted values when \eqn{t = 0}. If only one scenario is desired then \code{scen} should be a data frame. If more than one scenario is desired then the \eqn{t = 0} values should be in data frames contained in a list.
#' @param n numeric. Specifies the number of iterations (or time period) over which the program will generate the predicted value of the dependent variable. The default is 10.
#' @param sig numeric. Specifies the level of statistical significance of the confidence intervals. Any value allowed be greater than 0 and cannot be greater than 1.
#'
#' @param shocks data frame. Allows the user to choose independent variables (and its first \code{n} values) and have the variable (and potentially different values) impact the scenarios at each simulation. The number of values assigned to the shock variable must exceed the number of simulations. If this command is specified, the user must specify the \code{n} shock values with \code{shock_num}. If the shock variable is interacted with another variable in the model, the user must also specify the name of the modifying variable (\code{modify}) and the interaction variable (\code{inter}).
#' 
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
#' # Keep only variables included in the model 
#' # NOTE: Addresses weird Zelig issue that needs to be smoothed out.
#' grunSub <- grunfeld[, c("invest", "InvestLag", "mvalue", "kstock")]
#' 
#' # Estimate basic model 
#' M1 <- zelig(invest ~ InvestLag + mvalue + kstock, 
#'             model = "ls", data = grunfeld, cite = FALSE)
#'             
#' # Set up a senario
#' attach(grunSub) 
#' Scen1 <- data.frame(invest = 317, InvestLag = mean(InvestLag, na.rm = TRUE), 
#'                     mvalue = quantile(mvalue, 0.05), kstock = quantile(kstock, 0.05))
#' Scen2 <- data.frame(invest = 100, InvestLag = mean(InvestLag, na.rm = TRUE), 
#'                     mvalue = mean(mvalue), kstock = mean(kstock))
#' Scen3 <- data.frame(invest = 317, InvestLag = mean(InvestLag, na.rm = TRUE), 
#'                     mvalue = quantile(mvalue, 0.95), kstock = quantile(kstock, 0.95))
#'                     detach(grunSub)
#'                     
#' # Combine into a single list
#' ScenComb <- list(Scen1, Scen2, Scen3)
#' 
#' ## Run dynamic simulations without shocks and only 1 scenario
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
#' @references 
#' Williams, L. K., & Whitten, G. D. (2011). Dynamic Simulations of Autoregressive Relationships. The Stata Journal, 11(4), 577-588.
#' 
#' Williams, L. K., & Whitten, G. D. (2012). But Wait, There's More! Maximizing Substantive Inferences from TSCS Models. Journal of International Money and Finance, 74(03), 685-693.
#' 
#' @importFrom DataCombine MoveFront
#'
#' @export

dynsim <- function(obj, ldv, scen, n = 10, sig = 0.95, shocks = NULL){
	# Make sure both shocks is a data frame and the first column of shocks is a variable called times.
	if (!is.null(shocks)){
		if (class(shocks) != "data.frame"){
			stop("Shocks must be a data frame.")
		}
		if (names(shocks)[1] != "times"){
			stop("The first variable of shocks must be called 'times' and contain the shock times.")
		}
	}
	# Error if number of iterations is <= 0.
	if (n <= 0){
		stop("You must specify at least 1 iteration with the n argument.")
	}
	# Make sure sig is between 0 and 1.
	if (sig <= 0 | sig > 1){
		stop("sig must be greater than 0 and not greater than 1.")
	}

	# Determine if 1 or more scenarios are desired and simulate scenarios
	if (class(scen) == "data.frame"){
		SimOut <- OneScen(obj = obj, ldv = ldv, n = n, scen = scen, sig = sig, 
						  shocks = shocks)
	}
	else if (class(scen) == "list"){
		SimOut <- data.frame()
		scenNum <- length(scen)
		for (u in 1:scenNum){
			ScenTemp <- scen[[u]]
			SimTemp <- OneScen(obj = obj, ldv = ldv, n = n, scen = ScenTemp, sig = sig, shocks = shocks)
			SimTemp$scenNumber <- u
			SimTemp <- MoveFront(SimTemp, "scenNumber")
			SimOut <- rbind(SimOut, SimTemp)
		}
	}

	# Ascribe class and return output
	class(SimOut) <- "Dynsim"
	return(SimOut)
}






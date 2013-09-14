#' Dynamically simulations of autoregressive relationships
#' 
#' \code{dynsim} dynamic simulations of autoregressive relationships
#' 
#' @param ldv character. Names the lagged dependent variable
#' @parm scen1 vector. Specifies the values of the variables used to generate the predicted values when \eqn{t = 0}.
#' @param scen24 THINK ABOUT
#' @param sig numeric. Specifies the level of statistical significance of the confidence intervals.
#' 
#' @param modify character vector. The name of up to four variables that modify the relationship between the shock vari- able and the dependent variable. 
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
#' @export

dynsim <- function(ldv, scen1, scen24, modify = NULL, inter = NULL){
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
}
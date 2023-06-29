#' Fund Capital Allocation
#'
#' @description
#' This function allows the analyst to solve for the initial seed capital given contract specifications for capital across forward periods needed, all to match cashflow requirements in forward periods. Input to a canonical linear programming model are a cashflow funding matrix with rows of forward claims periods and columns of funding contracts, and a vector of forward claims requirements. The function specifies the objective function and the direction of requirements constraints in each forward period. The solution is the amount of capital needed to match claims in each period of claims experience and contract duration. The objective value is simply the initial capital needed to seed the fund.
#'
#' @param funds_flow A matrix of columns, one for each funding contract and rows, one for each forward period. Each cell with have values for 0 cashflow, -1 unit investment in a contract, and (1+rate)^duration, the unit forward value given a period rate and period duration of the contract.
#' @param requirements A vector of forward cashflow requirements of length equal to nrow(funds_flow).
#' @returns An unordered list of input funds flow, requirements, solution, and objective value.
#' @import lpsolve
#' @examples
#' # example code
#' library(lpSolve)
#' funds_flow <- matrix (c(-1,   0,    0,  0,
#'                        1.1,  -1,   -1,  0,
#'                        0,  1.11,    0, -1,
#'                        0,     0,    0,  1.18,
#'                        0,     0, 1.52,  0
#'                        ), nrow=5, byrow=TRUE)
#' requirements <- c(7, 5, 25, 11)
#' funding <- fund_capital(
#'                funds_flow,
#'                requirements)
#' funding$solution
#' funding$sensitivity
#' # The shadow prices for each period correctly compute the discount factors for each funding period
#'
fund_capital <- function( funds_flow, requirements){
  # invest during the first year, so no constraint on year-1
  X <- funds_flow[-1,]
  K <- ncol(X)
  N <- nrow(X)
  # we keep X as this will be augmented in a goal programming format in another function
  A <- X
  const_type <- c( rep(">=", N) )
  b <- requirements
  c <- c( 1, rep(0, K-1) )
  model <- lp( "min",
                 c,
                 A,
                 const_type,
                 b,
                 compute.sens=TRUE
                )
  result <- list(
    funds_flow = funds_flow,
    requirements = requirements,
    value = model$objval,
    solution = model$solution,
    sensitivity = model$sens.coef.from
  )
  return( result )
}

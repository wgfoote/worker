#' Funds Flow
#'
#' @description
#' We face a need to self-insure workers' compensation claims. This function helps an analyst to build a matrix rows of forward period unit cashflow with columns of funding contracts which specify forward rates, start and finish dates. Returned are the initial contract specification and the cashflow matrix.
#'
#' @param contracts A tibble of three columns: the name of the contracts, the period beginning the contract, the duration of the contract in periods, the period rate of return on the contracts; and rows for each financial contract contracts
#'
#' @param requirements A vector of forward period requirements. The first period is the initial capitalization of the fund. Requirements begin with the second period, which is the first forward period after the initial capitalization.

#' @returns List of the initial contract specification and a funds flow matrix with nrows = number of contracts, ncols = number of periods indicated by the start and duration of contracts + 1 for the name of the contracts, with column names the list of period labels
#' @import tidyverse
#' @examples
#' # example code
#' library( tidyverse )
#' contracts <- tibble(
#' name = c( "A", "B", "C", "D" ),
#' start = c( 1, 2, 2, 3 ),
#' duration = c( 1, 1, 3, 1),
#' rate = c( 0.10, 0.11, 0.15, 0.18)
#' )
#' requirements <- c( 7, 5, 25, 11 )
#' fund_flow( contracts, requirements )$funds_flow
#'
fund_flow <- function( contracts, requirements ) {
  # calculate period each contracts expires with return payoff per unit of input capital
  contracts <- contracts |>
  mutate( finish = start + duration,
          unit_value = ( 1 + rate )^duration
          ) |>
    arrange(start)
  # number of periods indicated by contracts specifications
  periods <- 1:max( contracts$finish )
  # catch differences between contract specifications and requirements
  if( length(requirements)!=length(periods)-1)
     stop( "Error: number of periods must match the requirements vector" )
  funds_flow <- matrix( 0, nrow=max(periods), ncol=length(contracts$start) )
  colnames( funds_flow ) <- contracts$name
  # unpack for convenience only
  start <- contracts$start
  finish <- contracts$finish
  unit_value <- contracts$unit_value
  for( i in 1:length(periods) ){
    for( j in 1:length(start) ) {
      funds_flow[i,j] <- ifelse( start[j]==i,
                              -1,
                              ifelse( finish[j]==i,
                                      unit_value[j],
                                      0
                                      )
                              )
      }
  }
  result <- list(
    contracts = contracts,
    requirements = requirements,
    funds_flow = funds_flow
  )
  return( result )
}

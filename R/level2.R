#' @export
level2 = function(a, b, tolerance = .05){
  #a and b are seperate alternatives in tibble/dataframe format with cost and value columns
  #b is the choosen alternative and a is the competitor with LOWER value
  #tolerance is the percentage less expected value the DM would be ok with at the expected cost

  b.value.mean = mean(b$value)
  b.cost.mean = mean(b$cost)
  a.value.mean = mean(a$value)
  a.cost.mean = mean(a$cost)


  #trade1 is the value/cost trade-off that you are considering
  trade1 = (b.value.mean - a.value.mean)/(b.cost.mean - a.cost.mean)
  m1 = atan((b.cost.mean - a.cost.mean)/(b.value.mean - a.value.mean))

  #trade2 is the point at which you would no longer make the value/cost trade off and go with the cheaper alternative
  #This is the minimum acceptable trade ... default at 5% less than expected mean
  trade2 = ((b.value.mean - a.value.mean)*(1-tolerance))/(b.cost.mean - a.cost.mean)
  m2 = atan((b.cost.mean - a.cost.mean)/((b.value.mean - a.value.mean)*(1-tolerance)))

  #bind alternatives for computation
  pairings = dpylr::tibble(a.value = a.value.mean, a.cost = a.cost.mean, b.value = b$value, b.cost = b$cost)

  #compute Zone 6 - Trade worse than expected - Pareto Optimal but worse than A
  pairings = dplyr::mutate(pairings, zone6 = dplyr::if_else((pairings$b.value < a.value.mean & pairings$b.cost < a.cost.mean), 1, 0))
  #compute Zone 5 - Trade much worse than expected - B dominated by A
  pairings = dplyr::mutate(pairings, zone5 = dplyr::if_else(pairings$b.value < a.value.mean & pairings$b.cost > a.cost.mean, 1, 0))
  #compute Zone 4 - Trade worse than expected - Unacceptable
  pairings = dplyr::mutate(pairings, zone4 = dplyr::if_else((pairings$b.value-a.value.mean)/(pairings$b.cost-a.cost.mean) < trade2 & pairings$b.value > a.value.mean & pairings$b.cost > a.cost.mean, 1, 0))
  #compute Zone 3 - Trade worse than expected - Acceptable
  pairings = dplyr::mutate(pairings, zone3 = dplyr::if_else(trade2 < (pairings$b.value-a.value.mean)/(pairings$b.cost-a.cost.mean) & (pairings$b.value-a.value.mean)/(pairings$b.cost-a.cost.mean) < trade1, 1, 0))
  #compute Zone 2 - Trade better than expected
  pairings = dplyr::mutate(pairings, zone2 = dplyr::if_else((pairings$b.value-a.value.mean)/(pairings$b.cost-a.cost.mean) > trade1 & pairings$b.cost > a.cost.mean, 1, 0))
  #compute Zone 1 - Trade better than expected - B dominates A
  pairings = dplyr::mutate(pairings, zone1 = dplyr::if_else(pairings$b.value > a.value.mean & pairings$b.cost < a.cost.mean, 1, 0))


  #Compute zones in place in single column named zoneTest for histogram building
  pairings = dplyr::mutate(pairings, zoneTest = dplyr::if_else(zone1 == 1, 1,
                                                 dplyr::if_else(zone2 == 1, 2,
                                                         dplyr::if_else(zone3 == 1, 3,
                                                                 dplyr::if_else(zone4 ==1, 4,
                                                                         dplyr::if_else(zone5 == 1, 5, 6)#end zones 5 and 6
                                                                 )#end zone 4
                                                         )#end zone 3
                                                 )#end zone 2
                                      )#end zone 1
  )#close zoneTest mutate

  return(pairings)
}

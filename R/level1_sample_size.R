#' @export
level1_sample_size = function(a, b, first_sample = 1000, confidence = .99, margin = .01){
  #a and b are seperate alternatives in tibble/dataframe format with cost and value columns
  #fist_sample has a default of 1000, but can be adjusted as desired
  #function scales well to 1,000,000 samples and still runs (slowly) at 10,000,000

  if(nrow(a) > first_sample){
    # #sample alternative
    a = a %>% dplyr::sample_n(first_sample, replace = TRUE)
  }
  if(nrow(b) > first_sample){
    # #sample alternative
    b = b %>% dplyr::sample_n(first_sample, replace = TRUE)
  }

  #create pairings
  pairings = merge(a,b, by = NULL)
  colnames(pairings) = c("alt.a","a.value","a.cost","alt.b","b.value","b.cost")

  #compute a dominance
  pairings = dplyr::mutate(pairings, a.dominate = dplyr::if_else(a.value > b.value & a.cost <= b.cost, 1, 0))
  #compute b dominance
  pairings = dplyr::mutate(pairings, b.dominate = dplyr::if_else(a.value < b.value & a.cost >= b.cost, 1, 0))
  #compute pareto optimal +
  pairings = dplyr::mutate(pairings, pareto_optimal_plus = dplyr::if_else(a.value >= b.value & a.cost >= b.cost, 1, 0))
  #compute pareto optimal -
  pairings = dplyr::mutate(pairings, pareto_optimal_minus = dplyr::if_else(a.value <= b.value & a.cost <= b.cost, 1, 0))
  #compute a greater than b value
  pairings = dplyr::mutate(pairings, a.GreaterThan.b = dplyr::if_else(a.value > b.value, 1, 0))
  #start building final table
  final_count = pairings %>% select(a.dominate, b.dominate, pareto_optimal_plus, pareto_optimal_minus)
  #sum observations
  final_count = final_count %>% colSums()
  final_table = as.data.frame(final_count)
  final_table = final_table %>% dplyr::mutate(percent = final_count/sum(final_count))
  #change col names to match the alternatives being examined
  row.names(final_table) = c(paste0(a$alternative[1],".dominate"), paste0(a$alternative[1],".dominated"), paste0(a$alternative[1],"/",b$alternative[1],".pareto_optimal_plus"),paste0(a$alternative[1],"/",b$alternative[1],".pareto_optimal_minus"))

  #Cochran's Formula for determining Sample Size Requirements
  #https://archive.org/details/Cochran1977SamplingTechniques_201703/page/n91
  #https://www.statisticshowto.datasciencecentral.com/probability-and-statistics/find-sample-size/
  #find z value
  z = -(qnorm((1-confidence)/2))
  #margin of error
  E = margin/2
  #test percentage
  p = final_table$percent[1]
  q = 1-p
  #determine sample requirement
  sample = ceiling(q*p*(z/E)^2)
  return(sample)
}

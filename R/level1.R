#' @export
level1 = function(a, b, max_combo= 10000, first_sample = 1000, confidence = .99, margin = .01, return_data = FALSE){
  #a and b are seperate alternatives in tibble/dataframe format with cost and value columns
  #first_sample has a default of 1000, but can be adjusted as desired
  #function scales well to 1,000,000 samples and still runs (slowly) at 10,000,000

  sample <- max(nrow(a),nrow(b))
  if(nrow(a)*nrow(b) > max_combo){
    sample <- level1_sample_size(a, b, first_sample, confidence, margin)
    print(paste0("Sample size of ", sample," pairings selected by Cochran's formula"))
  }else{
    print("Max combinations not reached.")
  }

  #if we have more observations than need for accurate sampling, reduce sets
  #if unequal observation sets, resample to largest length
  if(nrow(a) != sample){
    #sample alternative
    a = a %>% dplyr::sample_n(sample, replace = TRUE)
  }
  if(nrow(b) != sample){
    #sample alternative
    b = b %>% dplyr::sample_n(sample, replace = TRUE)
  }



  #create pairings
  pairings = cbind(a,b)
  colnames(pairings) = c("alt.a","a.value","a.cost","alt.b","b.value","b.cost")

  #compute a dominance
  pairings = dplyr::mutate(pairings, a.dominate = if_else(a.value > b.value & a.cost <= b.cost, 1, 0))
  #compute b dominance
  pairings = dplyr::mutate(pairings, b.dominate = if_else(a.value < b.value & a.cost >= b.cost, 1, 0))
  #compute pareto optimal +
  pairings = dplyr::mutate(pairings, pareto_optimal_plus = if_else(a.value >= b.value & a.cost >= b.cost, 1, 0))
  #compute pareto optimal -
  pairings = dplyr::mutate(pairings, pareto_optimal_minus = if_else(a.value <= b.value & a.cost <= b.cost, 1, 0))
  #compute a greater than b value
  pairings = dplyr::mutate(pairings, a.GreaterThan.b = if_else(a.value > b.value, 1, 0))
  #start building final table
  final_count = pairings %>%dplyr::select(a.dominate, b.dominate, pareto_optimal_plus, pareto_optimal_minus)
  #sum observations
  final_count = final_count %>% colSums()
  final_table = as.data.frame(final_count)
  final_table = final_table %>% dplyr::mutate(percent = final_count/sum(final_count))
  #change col names to match the alternatives being examined
  row.names(final_table) = c(paste0(a$alternative[1],".dominate"), paste0(a$alternative[1],".dominated"), paste0(a$alternative[1],"/",b$alternative[1],".pareto_optimal_plus"),paste0(a$alternative[1],"/",b$alternative[1],".pareto_optimal_minus"))

  dplyr::if_else(return_data == TRUE,
          return(list("final_table" = final_table, "pairings" = pairings)),
          return(final_table)
  )
}

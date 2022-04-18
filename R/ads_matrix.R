#' @export
ads_matrix = function(list_of_alt, maxlength = 1000){

  #build square matrix of zeros the size of the deminsions of your list
  ads_mat = matrix(0, nrow = length(list_of_alt), ncol = length(list_of_alt))

  #create dummy row/col headers so that we can dynamically fill it later
  dimnames(ads_mat) <- list(rownames(ads_mat, do.NULL = FALSE, prefix = "row"),
                            colnames(ads_mat, do.NULL = FALSE, prefix = "col"))

  #fill matrix by iterating through list, computing ads_score of each alternative against all others
  for (i in 1:length(list_of_alt)) {
    row.names(ads_mat)[i] = c(list_of_alt[[i]]$alternative[1])
    for (j in 1:length(list_of_alt)) {
      ads_mat[i,j] = as.numeric(ads_score(list_of_alt[[i]],list_of_alt[[j]],maxlength))
      colnames(ads_mat)[j] = c(list_of_alt[[j]]$alternative[1])
    }
  }

  #create new row and add rowsums to it
  ads_matrix_final = dplyr::as_data_frame(ads_mat) %>% dplyr::mutate(ads_score = rowSums(.)/(length(list_of_alt)-1))
  #add back in row names that dropped when converting to dataframe
  rownames(ads_matrix_final) = rownames(ads_mat)

  return(ads_matrix_final)

}


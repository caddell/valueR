#' @export
ads_score = function(a, b, maxlength = 1000){
  temp = level1(a,b, maxlength)
  score = (temp$final_count[1]-temp$final_count[2])/sum(temp$final_count)
  score = set_names(score,paste0(a$alternative[1], " compared to ", b$alternative[1]))
  return(as.data.frame(score))
}

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
  pairings = tibble(a.value = a.value.mean, a.cost = a.cost.mean, b.value = b$value, b.cost = b$cost)

  #compute Zone 6 - Trade worse than expected - Pareto Optimal but worse than A
  pairings = mutate(pairings, zone6 = if_else((pairings$b.value < a.value.mean & pairings$b.cost < a.cost.mean), 1, 0))
  #compute Zone 5 - Trade much worse than expected - B dominated by A
  pairings = mutate(pairings, zone5 = if_else(pairings$b.value < a.value.mean & pairings$b.cost > a.cost.mean, 1, 0))
  #compute Zone 4 - Trade worse than expected - Unacceptable
  pairings = mutate(pairings, zone4 = if_else((pairings$b.value-a.value.mean)/(pairings$b.cost-a.cost.mean) < trade2 & pairings$b.value > a.value.mean & pairings$b.cost > a.cost.mean, 1, 0))
  #compute Zone 3 - Trade worse than expected - Acceptable
  pairings = mutate(pairings, zone3 = if_else(trade2 < (pairings$b.value-a.value.mean)/(pairings$b.cost-a.cost.mean) & (pairings$b.value-a.value.mean)/(pairings$b.cost-a.cost.mean) < trade1, 1, 0))
  #compute Zone 2 - Trade better than expected
  pairings = mutate(pairings, zone2 = if_else((pairings$b.value-a.value.mean)/(pairings$b.cost-a.cost.mean) > trade1 & pairings$b.cost > a.cost.mean, 1, 0))
  #compute Zone 1 - Trade better than expected - B dominates A
  pairings = mutate(pairings, zone1 = if_else(pairings$b.value > a.value.mean & pairings$b.cost < a.cost.mean, 1, 0))



  #Compute zones in place in single column named zoneTest for histogram building
  pairings = mutate(pairings, zoneTest = if_else(zone1 == 1, 1,
                                                 if_else(zone2 == 1, 2,
                                                         if_else(zone3 == 1, 3,
                                                                 if_else(zone4 ==1, 4,
                                                                         if_else(zone5 == 1, 5, 6)#end zones 5 and 6
                                                                 )#end zone 4
                                                         )#end zone 3
                                                 )#end zone 2
                                )#end zone 1
  )#close zoneTest mutate



  #plot alternative b w/ densities with expected values of alternative A
  plot_trade= ggplot2::ggplot(data = b, aes(x = cost, y = value, color = alternative, fill = alternative))+
    ggthemes::theme_gdocs()+
    ggthemes::scale_color_gdocs()+
    ggthemes::scale_fill_gdocs()+
    ggplot2::stat_density2d(aes(alpha=..level.., color = alternative),geom = "polygon")+
    ggplot2::geom_point(alpha = .2)+
    ggplot2::geom_point(aes(x = mean(a$cost), y = mean(a$value), size = 3),color = "black",fill = "grey")+
    ggplot2::geom_vline(xintercept=mean(a$cost), linetype="dashed", size = 1)+
    ggplot2::geom_hline(yintercept=mean(a$value), linetype="dashed", size = 1)+
    ggplot2::geom_spoke(aes(x=a.cost.mean, y = a.value.mean, angle = -m1+(pi/2)), radius = sqrt((mean(a$value)-max(b$value))^2 + (mean(a$cost)-max(b$cost))^2),color = "black", size = 1.5,arrow=arrow(length = unit(0.5, "cm")))+
    ggplot2::geom_spoke(aes(x=a.cost.mean, y = a.value.mean, angle = -m2+(pi/2)), radius = sqrt((mean(a$value)-max(b$value))^2 + (mean(a$cost)-max(b$cost))^2), linetype="dashed",color = "black", size = 1,arrow=arrow(length = unit(0.5, "cm")))+
    ggplot2::annotate("text", label = paste0("Alternative ",a$alternative[1]), x = mean(a$cost)-4, y = mean(a$value)-1, size = 3, colour = "black")+
    ggplot2::labs(x = "Cost", y = "Value", title = "Uncertain Alternative Outcomes", fill= "Alternative")+
    ggplot2::guides(color = FALSE, alpha = FALSE, size = FALSE)+
    ggplot2::theme(plot.background=element_blank())+
    ggplot2::scale_fill_manual(values = c("red"))+
    ggplot2::scale_color_manual(values = c("red"))+
    ggplot2::coord_fixed()

  #histogram plot as % of total count
  plot_hist = ggplot2::ggplot(pairings,aes(zoneTest, fill = factor(zoneTest)))+
    ggthemes::theme_gdocs()+
    ggplot2::theme(plot.background=element_blank())+
    ggthemes::scale_color_colorblind()+
    ggplot2::geom_bar(aes(y = (..count..)/sum(..count..)))+
    ggplot2::labs(title = "Level 2 Analysis - Zones of Consideration", fill = "Zone", y = "% of Total Trades")+
    ggplot2::scale_fill_manual(name="Trade Zones",
                      breaks=c("1", "2", "3", "4", "5","6"),
                      labels=c("Better Than Expected - Dominates", "Better Than Expected Trade", "Worse Than Expected - Acceptable", "Worse Than Expected, Unacceptable", "Much Worse Than Expected - Dominated", "Worse Than Expected - Pareto Optimal Minus"),
                      values = c("green4","yellow2","red2","red3","red4","black"))

  return(list("trade" = plot_trade, "hist" = plot_hist,"pairings" = pairings))
}

library(tidyverse)
library(ggthemes)

set.seed(123)
n = 1000

orange = tibble(
  alternative = c(rep("orange", n)),
  value = c(rnorm(n, mean= 67, sd=3)),
  cost = c(rnorm(n, mean = 59, sd= 4))
)

green = tibble(
  alternative = c(rep("green", n)),
  value = c(rnorm(n, mean= 75, sd=3)),
  cost = c(rnorm(n, mean = 69, sd= 3))
)

blue = tibble(
  alternative = c(rep("blue", n)),
  value = c(rnorm(n, mean= 63, sd=2)),
  cost = c(rnorm(n, mean = 75, sd= 2))
)

yellow = tibble(
  alternative = c(rep("yellow", n)),
  value = c(rnorm(n, mean= 71, sd=1)),
  cost = c(rnorm(n, mean = 67, sd=1))
)

purple = tibble(
  alternative = c(rep("purple", n)),
  value = c(rnorm(n, mean= 74, sd=1.5)),
  cost = c(rnorm(n, mean = 72.5, sd=1.25))
)

red = tibble(
  alternative = c(rep("red", n)),
  value = c(rnorm(n, mean= 80, sd=1.5)),
  cost = c(rnorm(n, mean = 70, sd=2.25))
)

#bind all alternatives together for future use
all.alternatives = rbind(orange, green, blue, yellow, purple, red)


level1_red_blue = level1(red, blue, 500)

level1_red_blue$final_table


expectations = level2(green,red, .25)
expectations$trade
expectations$hist

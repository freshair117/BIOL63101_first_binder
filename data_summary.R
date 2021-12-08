library(tidyverse)

mpg %>%
 group_by(manufacturer) %>%
 summarize(mean_hwy = mean(hwy), sd_hwy = sd(hwy), number = n()) %>%
 arrange(-mean_hwy)

mpg %>%
 group_by(manufacturer) %>%
 summarize_at(c("displ", "cty", "hwy"), mean, na.rm = TRUE)

mpg %>%
 group_by(manufacturer) %>%
 summarize_if(is.numeric, mean, na.rm = TRUE)

mpg_with_mean <- mpg %>%
 group_by(manufacturer) %>%
 mutate(mean_hwy = mean(hwy), sd_hwy = sd(hwy)) %>%
 select(-class, -trans)

head(mpg_with_mean)

str(mpg_with_mean)




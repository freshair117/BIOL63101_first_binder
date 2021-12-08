library(tidyverse)

head(mpg)

str(mpg)

mpg %>%
 select(manufacturer)

mpg %>%
 distinct(manufacturer) %>%
 count()

mpg %>%
 filter(manufacturer == "honda" & year == "1999") %>%
 select(manufacturer, year, cty, hwy)

mpg %>%
 mutate(manufacturer = str_to_title(manufacturer), 
        model = str_to_title(model)) %>%
 select(manufacturer, model, year, trans, hwy)

my_messy_data <- read_csv("https://raw.githubusercontent.com/ajstewartlang/03_data_wrangling/master/data/my_data.csv")

head(my_messy_data)

my_tidied_data <- my_messy_data %>%
 mutate(condition = recode(condition,
                           "1" = "PrimeA_TargetA",
                           "2" = "PrimeA_TargetB",
                           "3" = "PrimeB_TargetA",
                           "4" = "PrimeB_TargetB")) %>%
 separate(col = "condition", into = c("Prime","Target"), sep = "_") %>%
 mutate(Prime = factor(Prime), Target = factor(Target))




my_wide_data <- read_csv("https://raw.githubusercontent.com/ajstewartlang/03_data_wrangling/master/data/my_wide_data.csv")

head(my_wide_data)

my_longer_data <- my_wide_data %>%
 pivot_longer(cols = c(Condition1, Condition2, Condition3, Condition4),
              names_to = "Condition",
              values_to = "RT")

head(my_longer_data)

my_longer_data %>%
 mutate(Condition = factor(Condition)) %>%
 head()

my_wider_data <- my_longer_data %>%
 pivot_wider(names_from = "Condition",
             values_from = "RT")

head(my_wider_data)


individual_diffs <- read_csv("https://raw.githubusercontent.com/ajstewartlang/03_data_wrangling/master/data/individual_diffs.csv")

head(individual_diffs)

combined_data <- full_join(my_longer_data, individual_diffs, by = "ID")

combined_data


large_ind_diffs <- read_csv("https://raw.githubusercontent.com/ajstewartlang/03_data_wrangling/master/data/large_ind_diffs.csv")

head(large_ind_diffs)

left_join(my_longer_data, large_ind_diffs, by = "ID")










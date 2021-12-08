library(tidyverse)
library(Hmisc)
library(ggridges)
library(NHANES)
library(ggplot2)
library(patchwork)



### 1 library(Hmisc) 
mpg %>%
 mutate (manufacturer = str_to_title(manufacturer)) %>%
 ggplot(aes(x = fct_reorder(manufacturer, .fun = mean, cty), y = cty, color = manufacturer)) +
 stat_summary(fun.data = mean_cl_boot, size = 1) +
 geom_jitter(alpha = .25) +
 theme_minimal() +
 theme(text = element_text(size = 13)) +
 labs(title = "Manufacturer by City Fuel Economy",
      x = "Manufacturer",
      y = "City Fuel Economy (mpg)") +
 guides(color = 'none') +
 coord_flip()

## 1.1 facet_wrap()
mpg %>%
 filter(class != "suv") %>%
 mutate(class = str_to_title(class)) %>%
 ggplot(aes(x = displ, y = cty, color = class)) +
 geom_jitter(width = .2) +
 theme_minimal() +
 theme(text = element_text(size = 13)) +
 labs(title = "City Fuel Economy by Engine Displacement",
      x = "Egine Displacement (litres)",
      y = "City Fuel Economy (mpg)") +
 guides(color = 'none') +
 facet_wrap(~class)
 

mpg %>%
 mutate(class = str_to_upper(class)) %>%
 ggplot(aes(x = cty, y = displ)) +
 geom_point(aes(color = class)) +
 geom_smooth(se = FALSE) +
 theme(text = element_text(size = 13)) +
 theme_minimal() +
 labs(x = "City Fuel Economy (mpg)",
      y = "Egine Displacement (litres)",
      color = "Vehicle Class")

mpg %>%
 ggplot(aes(x = displ)) +
 geom_histogram(binwidth = .5, fill = "grey") +
 labs(title = "Histogram of Engine Displacment",
      y = "Count",
      x = "Engine Displacement (litres)")

### 2 library(ggridges)
mpg%>%
 mutate(class = str_to_title(class)) %>%
 ggplot(aes(x = displ, y = fct_reorder(class, .fun = mean, displ))) +
 geom_density_ridges(height = .5, aes(fill = class)) +
 theme_minimal() +
 theme(text = element_text(size = 13)) +
 guides(fill = 'none') +
 labs(x = "Engine Displacement (litres)",
      y = NULL)

### 3 library(NHANES)
ncol(NHANES)

nrow(NHANES)

head(NHANES)

NHANES %>%
 select(ID) %>%
 n_distinct()

NHANES_tidied <- NHANES %>%
 distinct(ID, .keep_all = TRUE)

ncol(NHANES_tidied)

nrow(NHANES_tidied)

NHANES_tidied %>%
 ggplot(aes(x = BMI)) +
 geom_histogram(bins = 100, na.rm = TRUE)

NHANES_tidied %>%
 group_by(Education) %>%
 summarise(median = median(BMI, na.rm = TRUE))


## 3.1 geom_violin()
NHANES_tidied %>%
 filter(!is.na(Education) & !is.na(BMI)) %>%
 ggplot(aes(x = Education, y = BMI, color = Education)) +
 geom_violin() +
 geom_jitter(alpha = .2, width = .1) +
 geom_boxplot(alpha = .5) +
 guides(color = 'none') +
 labs(title = "Examining the effect of education level on BMI",
      x = "Education level",
      y = "BMI")

NHANES_tidied %>%
 filter(!is.na(Education) & !is.na(BMI) & !is.na(Diabetes)) %>%
 ggplot(aes(x = Education:Diabetes, y = BMI, color = Education)) +
 geom_violin() +
 geom_jitter(alpha = .2, width = .1) +
 geom_boxplot(alpha = .5) +
 guides(color = 'none') +
 theme(axis.text.x = element_text(angle = 45, vjust = 0.5)) +
 labs(title = "Examining the effect of education level and diabetes on BMI",
      x = "Education Level x Diabetes",
      y = "BMI")

## 3.2 Histograms with facet_wrap()
NHANES_tidied %>%
 filter(!is.na(Education) & !is.na(BMI)) %>%
 group_by(Education) %>%
 ggplot(aes(x = BMI, fill = Education)) +
 geom_histogram(aes(y = ..density..)) +
 geom_density(aes(y = ..density..)) +
 guides(fill = 'none') +
 #theme_minimal() +
 labs(title = "Examining the effect of education level on BMI",
      x = "BMI",
      #y = "Number of cases",
      y = "Density") +
 facet_wrap(~ Education
            , scales = "free"
            )

### 4 Challenge Part
plot11 <- NHANES_tidied %>%
 filter(!is.na(Diabetes) & !is.na(DiabetesAge)) %>%
 ggplot(aes(x = Race1:Diabetes, y = DiabetesAge, color = Race1)) +
 geom_violin(alpha = .5) +
 geom_jitter(alpha = .5, width = .05) +
 geom_boxplot(alpha = .5) +
 guides(color = 'none') +
 theme(text = element_text(size = 14)) +
 theme(axis.text.x = element_text(angle = 45, vjust = .5)) +
 labs(title = "Onset ages of diabetes across different races",
      x = "Race1 x Diabetes",
      y = "Onset age (Years)")


plot22 <- NHANES_tidied %>%
 filter(!is.na(BMI) & !is.na(Diabetes)) %>%
 mutate(gender = recode(Gender, "male" = "Male", "female" = "Female")) %>%
 ggplot(aes(x = Gender:Diabetes, y = BMI, color = Gender)) +
 geom_violin() +
 geom_jitter(alpha = .09, width = .1) +
 geom_boxplot(alpha = .15) +
 guides(color = 'none') +
 theme(text = element_text(size = 14)) +
 theme(axis.text.x = element_text(angle = 45, vjust = .5)) +
 labs(title = "Examining the gender difference and diabetes on BMI",
      x = "Gender x Diabetes",
      y = "BMI")

plot33 <- NHANES_tidied %>%
 filter(!is.na(HealthGen) & !is.na(Diabetes) & !is.na(DiabetesAge)
        & !is.na(AgeDecade)) %>%
 group_by(HealthGen) %>%
 ggplot(aes(x = DiabetesAge, fill = HealthGen)) +
 geom_histogram(
  #aes(y = ..density..)
  ) + 
 #geom_density(aes(y = ..density..)) +
 theme(text = element_text(size = 14)) +
 guides(fill = 'none') +
 labs(title = "Onset age from different health generations",
      x = "Onset age (Years)",
      y = "Number of cases"
      #, y = "Density"
      ) +
 facet_wrap(~ HealthGen)


all_plot <- (plot11 + plot22) / (plot33)

all_plot

ggsave("first_visualization_plot.jpg", plot = all_plot, width = 13, height = 15)






library(tidyverse) # Load the tidyverse packages
library(afex) # ANOVA functions
library(emmeans) # Needed for pairwise comparisons

# Reading Data
my_data <- read_csv("https://raw.githubusercontent.com/ajstewartlang/12_glm_anova_pt2/master/data/ancova_data.csv")

head(my_data)

# Code Condition as factor
my_data <- my_data %>%
 mutate(Condition = factor(Condition))
head(my_data)

# Summarising our Data
my_data %>%
 group_by(Condition) %>%
 summarise(mean_ability = mean(Ability))

# Visualising our Data
set.seed(1234)
ggplot(my_data, aes(x = Gaming, y = Ability, colour = Condition)) +
 geom_point(size = 3, alpha = .9) +
 labs(x = "Gaming Frequency (hours per week)",
      y = "Motor Ability") +
 theme_minimal() +
 theme(text = element_text(size = 11))

# Building our ANOVA model
anova_model <- aov_4(Ability ~ Condition + (1 | Participant), data = my_data)

anova(anova_model)

emmeans(anova_model, pairwise ~ Condition)

# ANCOVA
model_ancova <- aov_4(Ability ~ Gaming + Condition + (1 | Participant), data = my_data, factorize = FALSE)

anova(model_ancova)

emmeans(model_ancova, pairwise ~ Condition)

# ANOVA and ANCOVA in regression

# Visualising Data
my_data %>%
 ggplot(aes(x = Condition, y = Ability, color = Condition)) +
 geom_violin() +
 geom_jitter(width = .05, alpha = .8) +
 labs(x = "Condition",
      y = "Motor Ability") +
 stat_summary(fun.data = mean_cl_boot, color = "black") +
 guides(color = FALSE) +
 theme_minimal() +
 theme(text = element_text(size = 12))

# Setting up Contrasts
contrasts(my_data$Condition)

my_data <- my_data %>%
 mutate(Condition = fct_relevel(Condition, 
                                levels = c("Water", "Double Espresso", "Single Espresso")))

contrasts(my_data$Condition)

# ANOVA as Linear Model
model_lm <- lm(Ability ~ Condition, data = my_data)
model_lm

# ANCOVA as a Linear Model
model_ancova <- lm(Ability ~ Gaming + Condition, data = my_data)
model_ancova

# Gaming is not factor, so enter mean
mean(my_data$Gaming)

# Dummy Coding - water
Ability = Intercept + β1(Gaming) + β2(Double Espresso) + β3(Single Espresso)
Ability = -3.4498 + 0.8538(12.62296) + (- 1.0085)(0) + (-0.4563)(0)
Ability = -3.4498 + 10.777
Ability = 7.33

# Dummy Coding - Double Espresso
Ability = Intercept + β1(Gaming) + β2(Double Espresso) + β3(Single Espresso)
Ability = -3.4498 + 0.8538(12.62296) + (- 1.0085)(1) + (-0.4563)(0)
Ability = -3.4498 + 10.777 + (-1.0085)
Ability = 6.32

# Dummy Coding - Single Espresso
Ability = Intercept + β1(Gaming) + β2(Double Espresso) + β3(Single Espresso)
Ability = -3.4498 + 0.8538(12.62296) + (- 1.0085)(0) + (-0.4563)(1)
Ability = -3.4498 + 10.777 + (-0.4563)
Ability = 6.87

# Centering our Covariate
my_scaled_data <- my_data %>%
 mutate(centred_gaming = scale(Gaming))

 plot(density(my_scaled_data$Gaming))

 plot(density(my_scaled_data$centred_gaming))

# Build linear model with the scaled and centred covariate
 model_ancova_centred <- lm(Ability ~ centred_gaming + Condition, data = my_scaled_data)
 
model_ancova_centred



# Challenge Build Regression

# Challenge 1
ch_1 <- read_csv("https://raw.githubusercontent.com/ajstewartlang/11_glm_anova_pt1/master/data/ANOVA_data1.csv")
head(ch_1)

# Condition <chr> to <fct>
ch_1_ty <- ch_1 %>%
 mutate(Condition = factor(Condition))
head(ch_1_ty)

# Summarize data
ch_1_ty %>%
 group_by(Condition) %>%
 summarise(mean = mean(RT), sd = sd(RT))

# Visualize 1
ch_1_ty %>%
 ggplot(aes(x = Condition, y = RT, color = Condition)) +
 geom_violin() +
 geom_jitter(width = .1) +
 guides(color = 'none') +
 stat_summary(fun.data = "mean_cl_boot", color = "black")

# build ANOVA M1
M1 <- aov_4(RT ~ Condition + (1 | Subject), data = ch_1_ty)

# Interpret Model Output
summary(M1)

# Comparison
emmeans(M1, pairwise ~ Condition, adjust = "Bonferroni")
### Sig. Diff. in high vs low lexical freq. words, 
### (F(1,22) = 91.217, p <0.001, ges = .81),
### Reaction time in processing low lexical = higher

# Regression Model for ANOVA 2
contrasts(ch_1_ty$Condition)

contrasts(XXX1$Condition)

# ANOVA Linear Model
model_lm1 <- lm(RT ~ Condition, data = ch_1_ty)
model_lm1

high
Reaction Time = Intercept + β1(low)
Reaction Time = 864.7 + (313.5)(0)
Reaction Time = 864.7

low
Reaction Time = Intercept + β1(low)
Reaction Time = 864.7 + (313.5)(1)
Reaction Time = 1178.2


# Challenge 2
ch_2 <- read_csv("https://raw.githubusercontent.com/ajstewartlang/11_glm_anova_pt1/master/data/ANOVA_data2.csv")
head(ch_2)

# Condition <chr> to <fct>
ch_2_ty <- ch_2 %>%
 mutate(Condition = factor(Condition))
head(ch_2_ty)

# Group sum
ch_2_ty %>%
 group_by(Condition) %>%
 summarise(mean2 = mean(RT), sd2 = sd(RT))

# Visualize 2
ch_2_ty %>%
 ggplot(aes(x = fct_reorder(Condition, RT), y = RT, color = Condition)) +
 geom_violin() +
 geom_jitter(width = .1) +
 guides(color = 'none') +
 stat_summary(fun.data = "mean_cl_boot", color = "black") +
 xlab("Condition") +
 ylab("RT")

# build ANOVA M2
M2 <- aov_4(RT ~ Condition + (1 | Subject), data = ch_2_ty)

# Interpreting the Model Output
summary(M2)

# Comparison
emmeans(M2, pairwise ~ Condition, adjust = "Bonferroni")
### Sig. Diff. from all v.low * v.high lexical freq. words, 
### (F(3,44) = 203.21, p <0.001, ges = .93),
### Reaction time in processing v.low > low > high >v.high lexical 

# Building Regression ANOVA Part2
contrasts(ch_2_ty$Condition)

model_lm2 <- lm(RT ~ Condition, data = ch_2_ty)
model_lm2

# Dummy coding
high
Reaction Time = Intercept + β1(low) + β2(very high) + β3(very low)
Reaction Time = 927.1 + (237.6)(0) + (-314.4)(0) + (551.1)(0)
Reaction Time = 927.1

low
Reaction Time = Intercept + β1(low) + β2(very high) + β3(very low)
Reaction Time = 927.1 + (237.6)(1) + (-314.4)(0) + (551.1)(0)
Reaction Time = 927.1 + (237.6)
Reaction Time = 1164.7

very high
Reaction Time = Intercept + β1(low) + β2(very high) + β3(very low)
Reaction Time = 927.1 + (237.6)(0) + (-314.4)(1) + (551.1)(0)
Reaction Time = 927.1 + (-314.4)
Reaction Time = 612.7

very low
Reaction Time = Intercept + β1(low) + β2(very high) + β3(very low)
Reaction Time = 927.1 + (237.6)(0) + (-314.4)(0) + (551.1)(1)
Reaction Time = 927.1 + (551.1)
Reaction Time = 1478.2


library(tidytuesdayR)

transit_cost <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-01-05/transit_cost.csv')

transit_cost_ty <- transit_cost %>%
 mutate(country = factor(country))

transit_cost_ty1 <- transit_cost_ty
 group_by(country) %>%
 summarise(mean_kmmillions = mean(cost_km_millions))











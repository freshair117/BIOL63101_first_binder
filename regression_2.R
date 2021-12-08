library(tidyverse)
library(Hmisc)
library(MASS)
library(car)
library(olsrr)
library(performance)
library(tidytuesdayR)


MRes_tut2 <-read_csv("https://raw.githubusercontent.com/ajstewartlang/10_glm_regression_pt2/master/data/MRes_tut2.csv")

# Examining Possible Relationships

ggplot(MRes_tut2, aes(x = age, y = corr_spell)) +
 geom_point() +
 geom_smooth(method = "lm", se = FALSE) +
 theme_minimal() +
 theme(text = element_text (size = 13))

ggplot(MRes_tut2, aes(x = RA, y = corr_spell)) +
 geom_point() +
 geom_smooth(method = "lm", se = FALSE) +
 theme_minimal() +
 theme(text = element_text(size = 13))

ggplot(MRes_tut2, aes(x = std_SPELL, y = corr_spell)) + 
 geom_point() + 
 geom_smooth(method = "lm", se = FALSE) +
 theme_minimal() +
 theme(text = element_text(size = 13)) 

# Model the Data

model0 <- lm(corr_spell ~ 1, data = MRes_tut2)
model1 <- lm(corr_spell ~ age + RA + std_RA + std_SPELL, data = MRes_tut2)

anova(model0,model1)

check_model(model1)

# Dropping an Influential Case

MRes_tut2_drop10 <- filter(MRes_tut2, case != "10")

model12 <- lm(corr_spell~ age + RA + std_RA + std_SPELL, data = MRes_tut2_drop10)

check_model(model2)

vif(model2)

rcorr(MRes_tut2_drop10$RA, MRes_tut2_drop10$std_RA)

# Re(model) the Data

model3 <- lm(corr_spell ~ age + std_RA + std_SPELL, data = MRes_tut2_drop10)

vif(model3)

check_model(model3)

# Summary of Model

summary(model3)

model0 <- lm(corr_spell ~ 1, data = MRes_tut2_drop10)

anova(model3,model0)

# Equation:

#Spelled correct = -209.44 + 1.10(age) + 0.38(std_RA) + 1.21(std_SPELL) + residual


# Stepwise regression

model0 <- lm(corr_spell ~ 1, data = MRes_tut2_drop10)
model1 <- lm(corr_spell ~ age + std_RA + std_SPELL, data = MRes_tut2_drop10)

steplimitsf <- step(model0, scope = list (lower = model0, upper = model1),
                    direction = "forward")

summary(steplimitsf)

steplimitsb <- step(model1, direction = "back")

summary(steplimitsb)

steplimitsboth <- step(model0, scope = list (upper = model1),
                       direction = "both")

check_model(steplimitsboth)

pmodel <- ols_step_forward_p(model1)

pmodel


coffee_ratings <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-07-07/coffee_ratings.csv')





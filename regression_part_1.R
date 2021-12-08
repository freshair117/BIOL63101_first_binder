library(tidyverse)
library(Hmisc)
library(performance)
library(see)
library(patchwork)

crime <- read_csv("https://raw.githubusercontent.com/ajstewartlang/09_glm_regression_pt1/master/data/crime_dataset.csv")

head(crime)


crime <- separate(crime, col= "City, State", into = c("City", "State"))

head(crime)


crime <- crime %>%
 rename(House_price = index_nsa) %>%
 rename(Violent_Crimes = "Violent Crimes")

head(crime)


crime %>%
 ggplot(aes(x = Population, y = Violent_Crimes)) +
 geom_point() +
 geom_smooth(method = "lm", se = FALSE) +
 theme_minimal() +
 theme(text = element_text(size = 13)) +
 labs(x = "Population",
      y = "Violent Crimes")

# Pearson's r
rcorr(crime$Population, crime$Violent_Crimes)


crime_filtered <- filter(crime, Population < 2e+06)

crime_filtered %>%
 ggplot(aes(x = Population, y = Violent_Crimes)) +
 geom_point(alpha = .25) +
 geom_smooth(method = "lm", se = FALSE) +
 theme_minimal() +
 theme(text = element_text(size = 13)) +
 labs(x = "Population",
      y = "Violent Crimes")

rcorr(crime_filtered$Population, crime_filtered$Violent_Crimes)

crime_filtered <- filter(crime_filtered, Year == 2015)


crime_filtered %>%
 ggplot(aes(x = Population, y = Violent_Crimes, label = City)) +
 geom_point() +
 geom_text(nudge_y = 500, check_overlap = TRUE) +
 geom_smooth(method = "lm", se = FALSE) +
 xlim(0,1.8e6) +
 theme_minimal() +
 theme(text = element_text(size = 13)) +
 labs(x = "Population",
      y = "Violent Crimes")

rcorr(crime_filtered$Population, crime_filtered$Violent_Crimes)

# Model the Data
model1 <- lm(Violent_Crimes ~ 1, data = crime_filtered)
model2 <- lm(Violent_Crimes ~ Population, data = crime_filtered)

check_model(model2)

anova(model1, model2)

summary(model2)



### Challenge

# 1. Check whether the same relationship holds for population size and robberies in 2015.
model1.1 <- lm(Robberies ~ 1, data = crime_filtered)
model1.2 <- lm(Robberies ~ Population, data = crime_filtered)

check_model(model1.2)

anova(model1.1, model1.2)

summary(model1.2)

rcorr(crime_filtered$Population, crime_filtered$Robberies)


crime_filtered %>%
 ggplot(aes(x = Population, y = Robberies, label = City)) +
 geom_point() +
 geom_text(nudge_y = 500, check_overlap = TRUE) +
 geom_smooth(method = "lm", se = FALSE) +
 #xlim(0,1.8e6) +
 theme_minimal() +
 theme(text = element_text(size = 13)) +
 labs(x = "Population",
      y = "Robberies")



# 2. Are house prices predicted by the number of violent crimes in 2015?
model2.1 <- lm(House_price ~ 1, data = crime_filtered)
model2.2 <- lm(House_price ~ Violent_Crimes, data = crime_filtered)

check_model(model2.2)

anova(model2.1, model2.2)

summary(model2.2)

rcorr(crime_filtered$Violent_Crimes, crime_filtered$House_price)

crime_filtered %>%
 ggplot(aes(x = Violent_Crimes, y = House_price, label = City)) +
 geom_point() +
 geom_text(nudge_x = 500, check_overlap = TRUE) +
 geom_smooth(method = "lm", se = FALSE) +
 #xlim(0,1.8e6) +
 theme_minimal() +
 theme(text = element_text(size = 13)) +
 labs(x = "Violent Crimes",
      y = "House Price")


# 3. Are house prices predicted by population size in 2015?

model3.1 <- lm(House_price ~ 1, data = crime_filtered)
model3.2 <- lm(House_price ~ Population, data = crime_filtered)

check_model(model3.2)

anova(model3.1, model3.2)

summary(model3.2)

rcorr(crime_filtered$Population, crime_filtered$House_price)

crime_filtered %>%
 ggplot(aes(x = Population, y = House_price, label = City)) +
 geom_point() +
 geom_text(nudge_x = 500, check_overlap = TRUE) +
 geom_smooth(method = "lm", se = FALSE) +
 #xlim(0,1.8e6) +
 theme_minimal() +
 theme(text = element_text(size = 13)) +
 labs(x = "Population",
      y = "House price")






#xxxxx rubbish test

model2.11 <- lm(Violent_Crimes ~ 1, data = crime_filtered)
model2.22 <- lm(Violent_Crimes ~ House_price, data = crime_filtered)

check_model(model2.22)

anova(model2.11, model2.22)

summary(model2.22)

rcorr(crime_filtered$House_price, crime_filtered$Violent_Crimes)



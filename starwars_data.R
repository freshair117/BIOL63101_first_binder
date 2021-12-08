library(tidyverse)

view(starwars)

starwars_with_human_mean <- starwars %>%
 filter(!is.na(height)) %>%
 filter(species == "Human") %>%
 summarize(mean_height = mean(height))

head(starwars_with_human_mean) 


starwars_with_human_median <- starwars %>%
 filter(!is.na(height)) %>%
 filter(species == "Human") %>%
 summarize(median_height = median(height))

head(starwars_with_human_median) 



starwars_with_droid_mean <- starwars %>%
 filter(!is.na(height)) %>%
 filter(species == "Droid") %>%
 summarize(mean_height = mean(height))

head(starwars_with_droid_mean)


starwars_with_droid_median <- starwars %>%
 filter(!is.na(height)) %>%
 filter(species == "Droid") %>%
 summarize(median_height = median(height))

head(starwars_with_droid_median)




starwars %>%
 filter(!is.na(mass)) %>%
 filter(species == "Human") %>%
 summarize(mean_mass = mean(mass))




starwars_species_hmb_mean <- starwars %>%
 group_by(species) %>%
 summarize_at(c("height", "mass","birth_year"), mean, na.rm = TRUE)

head(starwars_species_hmb_mean)

starwars %>%
 group_by(eye_color) %>%
 summarize_at(c("height", "mass","birth_year"), mean, na.rm = TRUE)

starwars %>%
 group_by(sex) %>%
 summarize_at(c("height", "mass","birth_year"), mean, na.rm = TRUE)

starwars %>%
 group_by(gender) %>%
 summarize_at(c("height", "mass","birth_year"), mean, na.rm = TRUE)

starwars %>%
 group_by(homeworld) %>%
 summarize_at(c("height", "mass","birth_year"), mean, na.rm = TRUE)

starwars %>%
 group_by(skin_color) %>%
 summarize_at(c("height", "mass","birth_year"), mean, na.rm = TRUE)

starwars %>%
 group_by(hair_color) %>%
 summarize_at(c("height", "mass","birth_year"), mean, na.rm = TRUE)

starwars %>%
 group_by(films) %>%
 summarize_at(c("height", "mass","birth_year"), mean, na.rm = TRUE)


starwars_with_means <- starwars %>%
 mutate(mean_height = mean(!is.na(height)),
        mean_mass = mean(!is.na(mass))) %>%
 select(-skin_color, -eye_color, -hair_color, -films, -gender,
        -vehicles, -starships)

head(starwars_with_means)

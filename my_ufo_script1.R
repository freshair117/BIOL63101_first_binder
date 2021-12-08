library(tidyverse) # load the tidyverse
library(ggrepel) # needed to ensure text labels do not overlap
library(patchwork) # needed to combine our 4 plots at the end

# read in data
ufo_sightings <- read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-06-25/ufo_sightings.csv")

# plot of top 10 US states with number of sightings in each state
plot1 <- ufo_sightings %>%
  filter(!is.na(state)) %>%
  mutate(state = str_to_upper(state)) %>%
  group_by(state) %>%
  tally() %>%
  top_n(10) %>%
  ggplot(aes(x = reorder(state,n), y = n, fill = state)) +
  geom_col() +
  coord_flip() +
  guides(fill = "none") +
  labs(title = "Top 10 States for UFO Sightings",
       x = NULL,
       y = NULL) +
  ylim(0, 11000) +
  theme_minimal() +
  theme(text = element_text(size = 15))

# work out the top 10 states with most UFO sightings
top_states <- ufo_sightings %>%
  filter(!is.na(state)) %>%
  group_by(state) %>%
  tally() %>%
  top_n(10) %>%
  pull(state)

# work out the states within lat and long limits (i.e., exclude Alaska)
tidied_ufo <- ufo_sightings %>%
  filter(country == "us") %>%
  filter(latitude > 24 & latitude < 50)

# plot all sightings on a map of the US, with 10 top states colored
plot2 <- tidied_ufo %>%
  ggplot(aes(x = longitude, y = latitude)) +
  geom_point(size = .5, alpha = .25) +
  theme_void() +
  coord_cartesian() +
  labs(title = "Site of UFO sightings in the US") +
  guides(color = FALSE) +
  guides(fill = FALSE) +
  theme(text = element_text(size = 15))

# plot of top 10 UFO shapes in California
plot3 <- tidied_ufo %>%
  filter(state == "ca") %>%
  filter(ufo_shape != "other") %>%
  filter(ufo_shape != "unknown") %>%
  group_by(ufo_shape) %>%
  tally() %>%
  top_n(10) %>%
  mutate(ufo_shape = str_to_title(ufo_shape)) %>%
  ggplot(aes(x = reorder(ufo_shape, n), y = n, fill = ufo_shape)) +
  geom_col() +
  coord_flip() +
  guides(fill = "none") +
labs(title = "Top 10 UFO shapes spotted in California",
     x = NULL,
     y = NULL) +
  theme_minimal() +
  theme (text = element_text(size = 15))

# Put plots together
my_plot <- (plot1 + plot3) / (plot2)

my_plot

# Saving plots as a .jpg file
ggsave("ufo_plot.jpg", plot = my_plot, width = 12, height = 10)

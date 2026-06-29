###############################################################
# Netflix Data Analysis using R
# Author: Ali Maqsood
###############################################################

# Libraries


library(tidyverse)
library(janitor)
library(lubridate)


# Load Dataset

netflix <- read_csv("data/netflix_titles.csv")


# Initial Exploration


glimpse(netflix)
head(netflix)
summary(netflix)


# Data Cleaning

netflix <- clean_names(netflix)


# Missing Values


missing_data <- colSums(is.na(netflix))

missing_df <- data.frame(
  Column = names(missing_data),
  Missing = missing_data
)


# Plot 1: Movies vs TV Shows


movies_vs_tv_plot <-
  ggplot(netflix, aes(type)) +
  geom_bar(fill = "red") +
  labs(
    title = "Netflix Content Distribution",
    x = "Content Type",
    y = "Count"
  ) +
  theme_minimal()

movies_vs_tv_plot

ggsave(
  "plots/movies_vs_tv.png",
  plot = movies_vs_tv_plot,
  width = 8,
  height = 5
)


# Plot 2: Top Countries


top_countries <- netflix %>%
  separate_rows(country, sep = ", ") %>%
  count(country, sort = TRUE) %>%
  filter(!is.na(country)) %>%
  slice_head(n = 10)

top_countries_plot <-
  ggplot(
    top_countries,
    aes(reorder(country, n), n)
  ) +
  geom_col(fill = "steelblue") +
  coord_flip() +
  labs(
    title = "Top 10 Countries Producing Netflix Content",
    x = "Country",
    y = "Titles"
  ) +
  theme_minimal()

top_countries_plot

ggsave(
  "plots/top_countries.png",
  plot = top_countries_plot,
  width = 9,
  height = 6
)

# Plot 3: Releases Over Time


movies_by_year <- netflix %>%
  count(release_year)

release_plot <-
  ggplot(
    movies_by_year,
    aes(release_year, n)
  ) +
  geom_line(linewidth = 1) +
  geom_point() +
  labs(
    title = "Netflix Releases by Year",
    x = "Release Year",
    y = "Number of Titles"
  ) +
  theme_minimal()

release_plot

ggsave(
  "plots/releases_over_time.png",
  plot = release_plot,
  width = 9,
  height = 5
)

# Plot 4: Top Genres


top_genres <- netflix %>%
  separate_rows(listed_in, sep = ", ") %>%
  count(listed_in, sort = TRUE) %>%
  slice_head(n = 10)

genres_plot <-
  ggplot(
    top_genres,
    aes(reorder(listed_in, n), n)
  ) +
  geom_col(fill = "darkorange") +
  coord_flip() +
  labs(
    title = "Top 10 Netflix Genres",
    x = "Genre",
    y = "Titles"
  ) +
  theme_minimal()

genres_plot

ggsave(
  "plots/top_genres.png",
  plot = genres_plot,
  width = 9,
  height = 6
)

# Plot 5: Ratings


ratings <- netflix %>%
  count(rating, sort = TRUE)

ratings_plot <-
  ggplot(
    ratings,
    aes(reorder(rating, n), n)
  ) +
  geom_col(fill = "purple") +
  coord_flip() +
  labs(
    title = "Netflix Ratings Distribution",
    x = "Rating",
    y = "Titles"
  ) +
  theme_minimal()

ratings_plot

ggsave(
  "plots/ratings.png",
  plot = ratings_plot,
  width = 9,
  height = 6
)


# Plot 6: Movie Duration


movies <- netflix %>%
  filter(type == "Movie")

movies$minutes <- as.numeric(
  str_extract(movies$duration, "\\d+")
)

duration_plot <-
  ggplot(
    movies,
    aes(minutes)
  ) +
  geom_histogram(
    bins = 30,
    fill = "red",
    color = "white"
  ) +
  labs(
    title = "Distribution of Movie Durations",
    x = "Minutes",
    y = "Count"
  ) +
  theme_minimal()

duration_plot

ggsave(
  "plots/movie_duration.png",
  plot = duration_plot,
  width = 9,
  height = 6
)


# Plot 7: Top Directors

top_directors <- netflix %>%
  filter(!is.na(director)) %>%
  separate_rows(director, sep = ", ") %>%
  count(director, sort = TRUE) %>%
  slice_head(n = 10)

directors_plot <-
  ggplot(
    top_directors,
    aes(reorder(director, n), n)
  ) +
  geom_col(fill = "forestgreen") +
  coord_flip() +
  labs(
    title = "Top Directors on Netflix",
    x = "Director",
    y = "Titles"
  ) +
  theme_minimal()

directors_plot

ggsave(
  "plots/top_directors.png",
  plot = directors_plot,
  width = 9,
  height = 6
)

# Plot 8: Missing Values

missing_plot <-
  ggplot(
    missing_df,
    aes(reorder(Column, Missing), Missing)
  ) +
  geom_col(fill = "tomato") +
  coord_flip() +
  labs(
    title = "Missing Values by Column",
    x = "Column",
    y = "Missing Values"
  ) +
  theme_minimal()

missing_plot

ggsave(
  "plots/missing_values.png",
  plot = missing_plot,
  width = 9,
  height = 6
)
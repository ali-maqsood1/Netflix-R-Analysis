library(tidyverse)
library(janitor)
library(lubridate)

netflix <- read_csv("data/netflix_titles.csv")

glimpse(netflix)

head(netflix)

summary(netflix)

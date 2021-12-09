# Parse CSV

# install.packages("RSQLite")
# install.packages("dbplyr")
library(tidyverse)
library(DBI)
library(dbplyr)
library(dplyr)

# Create an ephemeral in-memory RSQLite database
con <- dbConnect(RSQLite::SQLite(), "data/pitchforkDB.db")

# Create each individual tibble
artists = collect(tbl(con, "artists"))
content = collect(tbl(con, "content"))
genres = collect(tbl(con, "genres"))
labels = collect(tbl(con, "labels"))
reviews = collect(tbl(con, "reviews"))
years = collect(tbl(con, "years"))


# create a new tibble with genres added to the reviews
genres_and_scores = left_join(reviews, genres, by = "reviewid") %>% 
  na.omit("genre") %>% 
  select(c("artist", "title", "score", "genre", "pub_year")) %>%
  group_by(artist) %>% 
  arrange(pub_year, .by_group = TRUE)
names(genres_and_scores) = c("Artist", "Album", "Score", "Genre", "Year Review Published")


# Disconnect from the database
dbDisconnect(con)

# Parse CSV

# install.packages("RSQLite")
# install.packages("dbplyr")
library(tidyverse)
library(DBI)
library(dbplyr)
library(dplyr)
library(lubridate)

# Create an ephemeral in-memory RSQLite database
con <- dbConnect(RSQLite::SQLite(), "data/pitchforkDB.db")

# Create each individual tibble
artists = collect(tbl(con, "artists"))
content = collect(tbl(con, "content"))
genres = collect(tbl(con, "genres"))
labels = collect(tbl(con, "labels"))
reviews = collect(tbl(con, "reviews"))
years = collect(tbl(con, "years"))


reviews$pub_date = date(reviews$pub_date)

# create a new tibble with genres added to the reviews
reviews_summary = left_join(reviews, genres, by = "reviewid") %>% 
  left_join(., labels, by = "reviewid") %>% 
  left_join(., years, by = "reviewid") %>%
  distinct(reviewid, .keep_all = TRUE) %>% 
  na.omit("genre") %>% 
  select(c("artist", "title", "score", "genre",  "label", "best_new_music", "year")) %>%
  group_by(artist)
  #arrange(., summary(as.factor(unique(label))))
  
  
names(reviews_summary) = c("Artist", "Album", "Score", "Genre", "Label", "Best New Music", "Year")

  

# Disconnect from the database
dbDisconnect(con)
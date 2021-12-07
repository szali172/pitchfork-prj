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

# Disconnect from the database
dbDisconnect(con)

library(tidyverse)

# When passed the genre input, check if "All" is selected or not
# If it is, return reviews_summary
# else, return filtered genre
check_input = function(genre) {
  if (genre == "All") {
    return(reviews_summary)
  }
  output = reviews_summary %>% 
    filter(`Genre` == genre)
  return(output)
}
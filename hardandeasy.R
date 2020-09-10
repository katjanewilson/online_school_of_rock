### amount of times that something says "hard" or "this is too hard"


# make another vector

# within comments_small, what is the amount of times it includes this string: hard or easy

hard_easy_data <- comments_small %>%
  mutate(hard_marker =  case_when(
    str_detect(textOriginal, "hard") |
      str_detect(textOriginal, "too hard") |
      str_detect(textOriginal, "really hard") ~ "hard" )) %>%
  mutate(easy_marker = case_when(
    str_detect(textOriginal, "too easy") |
      str_detect(textOriginal, "this is easy") ~ "easy"))
easy_frame <- hard_easy_data %>%
  filter(hard_marker == "hard" | easy_marker == "easy")

#match these with sentiment positivity score to see how hard and easy it gets
library(tidytext)
get_sentiments("afinn")

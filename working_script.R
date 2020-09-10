### step 1 - load

load("comments_small.Rdata")

## somehow make this into the "liklihood of mentioning this part of the guitar"

load("unnested_small.Rdata")
load("nrc.Rdata")

### step 2- nrc frames

library(tidyverse)
function_plot2 <- function(emotion) {
  nrc_joy <- nrc %>%
    filter(sentiment == emotion)
  onewords <- unnested_small %>%
    select(word)
  onewords <- onewords %>%
    inner_join(nrc_joy) %>%
    group_by(word) %>%
    summarise(total = n()) %>%
    arrange(total) %>%
    filter(total>20)
  plot <- ggplot(onewords) + geom_bar(mapping = aes(x = reorder(word, total), y=total, fill = total), stat = "identity") +
    coord_flip() +
    scale_fill_continuous(type = "viridis") +
    labs(title = emotion, x = "Count", y = "Word")+
    theme(plot.title = element_text(size = 11)) +
    theme(legend.title = element_blank(),
          legend.text = element_blank(),
          axis.text.x = element_blank())
}

plot1<- function_plot2("anger")
plot2 <- function_plot2("trust")
plot3 <- function_plot2("sadness")
plot4<- function_plot2("disgust")
plot5<- function_plot2("joy")
library(gridExtra)
grid.arrange(plot1, plot2, plot3, plot4, ncol=2)


### step 3- hard and easy

hard_easy_data <- comments_small %>%
  mutate(hard_marker =  case_when(
    str_detect(textOriginal, "hard") |
      str_detect(textOriginal, "not easy") |
      str_detect(textOriginal, "difficult") |
      str_detect(textOriginal, "too hard") |
      str_detect(textOriginal, "really hard") ~ "hard" )) %>%
  mutate(easy_marker = case_when(
    str_detect(textOriginal, "too easy") |
      str_detect(textOriginal, "not hard") |
      str_detect(textOriginal, "this is easy") ~ "easy")) %>%
  select(textOriginal, hard_marker, easy_marker, authorDisplayName)
        
write.csv(hard_easy_data, file = "data/hard_easy_data.csv")

### step 4 - afinn frames
#match these with sentiment positivity score to see how hard and easy it gets
library(tidytext)
get_sentiments("afinn")

hard_easy_data <- read.csv("data/hard_easy_data.csv")
hard_easy_data <- hard_easy_data %>%
  unnest_tokens(word, textOriginal)

hard_easy_data <- hard_easy_data %>%
  inner_join(get_sentiments("afinn")) %>%
  group_by(X) %>%
  mutate(means = mean(value))
match <- read.csv("data/hard_easy_data.csv")
joined <- merge(match, hard_easy_data, by = "X")



super_both <- joined %>%
  select(textOriginal, hard_marker.x, easy_marker.x, word, value, means) %>%
  mutate(nums = sapply(strsplit(textOriginal, " "), length)) %>%
  mutate(cureword = case_when(
    str_detect(textOriginal, "fuck") |
      str_detect(textOriginal, "bitch") ~ 1)) %>%
  mutate(thanks = case_when(
    str_detect(textOriginal, "thank you") |
      str_detect(textOriginal, "thanks") ~ 1)) %>%
  mutate(laughing = case_when(
    str_detect(textOriginal, "haha") |
      str_detect(textOriginal, "lol") ~ 1)) %>%
  mutate(Marty = case_when(
    str_detect(textOriginal, "Marty") |
      str_detect(textOriginal, "Schwartz") ~ 1)) %>%
  group_by(textOriginal) %>%
  mutate(comment_length = mean(nums), sentiment_score = mean(value)) %>%
  mutate(polarity_identifier = ifelse(sentiment_score >0, "positive", "negative")) %>%
  mutate(factor_sentiment = round(sentiment_score, digits = 0))%>%
  mutate(pentatonic = case_when(str_detect(textOriginal, "pentatonic") ~ 1)) %>%
  mutate(scales = case_when(str_detect(textOriginal, "scales") ~ 1)) %>%
  mutate(total_specifics = pentatonic + scales) %>%
  mutate(curse_word = case_when(
    str_detect(textOriginal, "fuck") |
      str_detect(textOriginal, "bitch") ~ 1)) %>%
  mutate(Marty = case_when(
    str_detect(textOriginal, "Marty") |
      str_detect(textOriginal, "Schwartz") ~ 1))%>%
  mutate(lists_any_technical_skill = case_when(
    str_detect(textOriginal, "pentatonic") |
      str_detect(textOriginal, "scales") |
      str_detect(textOriginal, "fret") |
      str_detect(textOriginal, "fretboard")|
      str_detect(textOriginal, "theory") |
      str_detect(textOriginal, "scale")~ 1)) %>%
  mutate(lists_any_technical_skill = ifelse(is.na(lists_any_technical_skill), 0, 1)) %>%
  mutate(scales = ifelse(is.na(scales), 0, 1)) %>%
  mutate(thanks = ifelse(is.na(thanks), 0, 1)) %>%
  mutate(pentatonic = ifelse(is.na(pentatonic), 0, 1)) %>%
  mutate(laughing = ifelse(is.na(laughing), 0, 1)) %>%
mutate(hard_word = ifelse(is.na(hard_marker.x), 0, 1))%>%
  mutate(Marty = ifelse(is.na(Marty), 0, 1)) %>%
  mutate(curse_word = ifelse(is.na(curse_word), 0, 1))%>%
  select(textOriginal, comment_length, polarity_identifier, pentatonic, scales,
         hard_word, word, value, curse_word, Marty, thanks, laughing, factor_sentiment, lists_any_technical_skill)
super_both$polarity_identifier <- as.factor(super_both$polarity_identifier)
p <- ggplot(data = super_both, aes(x=value, y = comment_length, color = polarity_identifier)) +
  geom_point(alpha = .08)
p
super_both$factor_sentiment <- as.factor(super_both$factor_sentiment)
p <- ggplot(data = super_both, aes(x=value, y = comment_length, color = factor_sentiment)) +
  geom_point(alpha = .2)
p
### do this at the comment level
super_both_comment_level <- super_both %>%
  group_by(textOriginal, factor_sentiment) %>%
  summarise(length = mean(comment_length))
p <- ggplot(data = super_both_comment_level, aes(x=factor_sentiment, y = length, color = factor_sentiment)) +
  geom_point(alpha = .2)
p
p <- ggplot(data = super_both_comment_level, aes(x=factor_sentiment,
                                                 y = length, 
                                                 color = factor_sentiment)) +
  geom_jitter(alpha = .1)
p


##plot these together

## hard word
super_both_comment_level_hard <- super_both %>%
  group_by(textOriginal, factor_sentiment, hard_word) %>%
  summarise(length = mean(comment_length))
super_both_comment_level_hard$hard_word <- as.factor(super_both_comment_level_hard$hard_word)
p1 <- ggplot(data = super_both_comment_level_hard, aes(x=factor_sentiment,
                                                      y = length)) +
  geom_jitter(aes(x=factor_sentiment,
                  y = length, color = hard_word), alpha = .3, size = 2) +
  ylim(0,100)+
  scale_color_manual(values = c("#A9D0F5", "#9F81F7")) +
  theme_minimal()
p1
## curse word
super_both_comment_level_curse <- super_both %>%
  group_by(textOriginal, factor_sentiment, curse_word) %>%
  summarise(length = mean(comment_length))
super_both_comment_level_curse$curse_word <- as.factor(super_both_comment_level_curse$curse_word)
p2 <- ggplot(data = super_both_comment_level_curse, aes(x=factor_sentiment, y = length)) +
  geom_jitter(aes(x=factor_sentiment,
                  y = length, color = curse_word), alpha = .3, size = 2) +
  ylim(0,100)+
  scale_color_manual(values = c("#A9D0F5", "#9F81F7")) +
  theme_minimal()
p2
## scales word
super_both_comment_level_technical <- super_both %>%
  group_by(textOriginal, factor_sentiment, lists_any_technical_skill) %>%
  summarise(length = mean(comment_length))
super_both_comment_level_technical$lists_any_technical_skill  <- as.factor(super_both_comment_level_technical$lists_any_technical_skill)
p3 <- ggplot(data = super_both_comment_level_technical, aes(x=factor_sentiment, y = length)) +
  geom_jitter(aes(x=factor_sentiment,
                  y = length, color = lists_any_technical_skill), alpha = .3, size = 2) +
  ylim(0,100)+
  scale_color_manual(values = c("#A9D0F5", "#9F81F7")) +
  theme_minimal()
p3


grid.arrange(p1,p2,p3, ncol=1)





## make this plotly for the specific ones

#these are individual words, so sometimes there are comments that are positive, blue, but 

# for instance, the comment "Kind of bummed that I have to trim my nails to 
#even come close to being able to do this.  lol  Damn it." this is a polarity of 1, so it is
# positive, but one of the specific words, "damn" apepars in the left hand side.

# you see more of these cures words appearing from the 1 positive people as a 
#joke, rather than the other way around. There are not many people who are
#underachievers who would say positive things. If they are 0 for polarity, they are
# relaly really out of it.

#these are your dark red dots at the bottom (the Shit)



freqs <- super_both %>%
  select(textOriginal, pentatonic, scales, hard_word, polarity_identifier,
         curse_word, Marty, thanks, laughing) %>%
  distinct() %>%
  group_by(polarity_identifier) %>%
  mutate(n = n()) %>%
  summarise(Pentatonic = sum(pentatonic)/n,
            Hard = sum(hard_word)/n,
            Scales = sum(scales)/n,
            Curse = sum(curse_word)/n,
            Marty= sum(Marty)/n,
            ThankYou= sum(thanks)/n,
            Laughter= sum(laughing)/n) %>%
  distinct()
try <- gather(freqs, 'Liklihood of Mentioning', meas, Pentatonic:Laughter) %>%
  spread(polarity_identifier, meas)
freqs

## with all factors
library(scales)
freqs_factor <- super_both %>%
  select(textOriginal, pentatonic, scales, hard_word, factor_sentiment,
         curse_word, Marty, thanks, laughing) %>%
  distinct() %>%
  group_by(factor_sentiment) %>%
  mutate(n = n()) %>%
  summarise(Pentatonic = sum(pentatonic)/n,
            Hard = sum(hard_word)/n,
            Scales = sum(scales)/n,
            Curse = sum(curse_word)/n,
            Marty= sum(Marty)/n,
            ThankYou= sum(thanks)/n,
            Laughter= sum(laughing)/n) %>%
  distinct()
try_factor <- gather(freqs_factor, 'Liklihood of Mentioning', meas, Pentatonic:Laughter) %>%
  spread(factor_sentiment, meas) 
try_factor$`1`<- percent(try_factor$`1`)
try_factor$`2`<- percent(try_factor$`2`)
try_factor$`3`<- percent(try_factor$`3`)
try_factor$`4`<- percent(try_factor$`4`)
try_factor$`-1`<- percent(try_factor$`-1`)
try_factor$`-2`<- percent(try_factor$`-2`)
try_factor$`-3`<- percent(try_factor$`-3`)
try_factor$`-4`<- percent(try_factor$`-4`)
try_factor$`0`<- percent(try_factor$`0`)
  
freqs


##test this
Marties_bad <- super_both %>%
  filter(polarity_identifier == 0) %>%
  select(textOriginal, Marty) %>%
  distinct()

table(Marties_bad$Marty)




#figure out how 

hard_easy_data_counts <- hard_easy_data %>%
  mutate(personals = str_count(textOriginal, c("I", "my")),
         marties = str_count(textOriginal, "Marty"))
ggplot(data = hard_easy_data_counts) +
  geom_point(aes(x = value, y = personals), alpha = .1)
ggplot(data = hard_easy_data_counts) +
  geom_point(aes(x = value, y = marties), alpha = .1)

### or you can try to do questions -- asking a GOOD question - the number of different words that are in a text

## the same words, if a curse word counts as the same, is likely to be a bad response-- not help the community

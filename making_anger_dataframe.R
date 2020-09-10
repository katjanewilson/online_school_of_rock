
## load data
load("comments_small.Rdata")
load("unnested_small.Rdata")
load("nrc.Rdata")

function_plot <- function(emotion) {
  nrc_joy <- nrc %>%
    filter(sentiment == emotion)
  onewords <- unnested_small %>%
    select(word)
  onewords <- onewords %>%
    inner_join(nrc_joy) %>%
    group_by(word) %>%
    summarise(total = n()) %>%
    arrange(desc(total))
  onewords <- onewords[1:10,]
  return(onewords)
}
function_plot("anger")



par(mfrow=c(2,2))
plot1
plot2
plot3


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
    filter(total>30)
  plot <- ggplot(onewords) + geom_bar(mapping = aes(x = reorder(word, total), y=total, fill = total), stat = "identity") +
    coord_flip() +
    scale_fill_continuous(type = "viridis") +
    labs(title = emotion, x = "Count", y = "Word")+
    theme(plot.title = element_text(size = 25)) +
    theme(legend.title = element_blank(),
          legend.text = element_blank())
  plot
}
function_plot2("anger")
function_plot2("trust")
function_plot2("sadness")
function_plot2("disgust")
function_plot2("joy")


#new line
load("comments_small.Rdata")
function_text <- function(newword) {
  onewords <- unnested_small %>%
    select(authorDisplayName, word)
  commentsdata <- comments_small %>%
    filter(grepl(newword, textOriginal)) %>%
    select(authorDisplayName, textOriginal)
  return(commentsdata)

  }

function_text("pentatonic")
function_text("scales")
function_text("pentatonic")
hurt_text <- hurt_text[1:5,]




trust_small <- trust[1:10,]
anger_small <- anger[1:10,]
write.csv(anger_small, file = "anger_small.csv")
write.csv(trust_small, file = "trust_small.csv")


install.packages("rjson")
library(rjson)
json_trust_small <- toJSON(trust_small)
write(json_trust_small, file = "json_trust_small.JSON")

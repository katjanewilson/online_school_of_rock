

function_plot <- function(emotion) {
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
    theme(plot.title = element_text(size = 25)) +
    theme(legend.title = element_blank(),
          legend.text = element_blank())
  plot
}

#new line
function_text <- function(newword) {
   onewords <- unnested_small %>%
    select(authorDisplayName, word)
  commentsdata <- comments_small %>%
    filter(grepl(newword, textOriginal)) %>%
    select(authorDisplayName, textOriginal)
  usq <- 0
  v <- length(commentsdata$authorDisplayName)
  for (i in 1:v) {
    usq[i] <- paste(commentsdata$authorDisplayName[i], ":", commentsdata$textOriginal[i])
    print(usq[i])
  }
}
function_plot("anger")
trust <- function_plot("trust")
function_text("hard")

write.csv(anger, file = "anger.csv", row.names = FALSE)
write.csv(trust, file = "trust.csv", row.names = FALSE)



data <- data.frame(sentiment = c("negative", "neutral", "positive", "negative", "neutral", "positive"), 
                  proportion = c(22, 103, 73, 49, 261, 102), 
                  version = c("one", "one", "one","two", "two", "two"))


data <- data.frame(sentiment = c("negative", "neutral", "positive", "negative", "neutral", "positive"), 
                   proportion = c(11, 52, 37, 12, 63, 25), 
                   version = c("one", "one", "one","two", "two", "two"))

ggplot(data=data, aes(x = version, y = proportion, fill = sentiment)) +
  geom_bar(stat="identity") + 
  scale_fill_manual(values = c("negative" = "#e18b22", 
                               "neutral" = "#46e7fd",
                               "positive" = "#4739a2"), 
                    guide_legend(title = "Sentiment")) + 
  labs(title = "Missing data", 
       subtitle = "Increases in drop out over time",
       caption = "Data source: Hypothetical case") + 
  xlab("Sentiment") + 
  ylab("Percentage") + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"), 
        plot.title = element_text(color = "#2F2E41", size = 12, face = "bold"),
        plot.subtitle = element_text(color = "#454543", face = "bold"),
        plot.caption = element_text(color = "#454543", face = "italic"))

# create a distribution plot of polarity scores
## ---- stacked-bar-chart

ggplot(app_all_sentiment, 
       aes(fill=sentiment, 
           y=proportion, 
           x=version)) + 
  geom_bar(position="stack", stat="identity") + 
  scale_fill_manual(values = c("negative" = "#e18b22", 
                               "neutral" = "#46e7fd", 
                               "positive" = "#4739a2"), 
                    labels = c("Positive", "Neutral", "Negative"), 
                    name = "Sentiment") +
  labs(title = paste0("Sentiment toward digital health tool"), 
       subtitle = "Stacked barchart comparing versions one and two",
       caption = "Data source: TeamBaby") +
  xlab("Version") + 
  ylab("Proportion") + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"), 
        plot.title = element_text(color = "#2F2E41", size = 12, face = "bold"),
        plot.subtitle = element_text(color = "#454543"),
        plot.caption = element_text(color = "#454543", face = "italic")
  )

## ---- average-log-ins

data_log_in <- final_data[!duplicated(final_data[ ,c('UserCode')]),]

data_log_in$log_in_count_group <- ifelse(data_log_in$log_ins == 1, 1, 
                                         ifelse(
                                           data_log_in$log_ins > 1 & 
                                             data_log_in$log_ins < 6, "2 to 5", 
                                           "6+"))

ggplot(data_log_in, aes(log_in_count_group)) + 
  geom_bar(aes(y = (..count..)/sum(..count..)), fill = "#454543") + 
  scale_y_continuous(labels=scales::percent) +
  labs(title = paste0("Frequency of engagement"), 
       subtitle = "Bar chart of log ins per user",
       caption = "Data source: TeamBaby") +
  xlab("Log ins") + 
  ylab("Proportion") + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"), 
        plot.title = element_text(color = "#2F2E41", size = 12, face = "bold"),
        plot.subtitle = element_text(color = "#454543"),
        plot.caption = element_text(color = "#454543", face = "italic")
  )

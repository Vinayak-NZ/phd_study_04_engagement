## ---- app-feedback-plot

# user-friendliness

ggplot(data = data_feedback) + 
  geom_bar(mapping = aes(x = ux, 
                         y = after_stat(prop), 
                         group = version), 
           stat = "count", 
           fill = "#454543") + 
  scale_y_continuous(labels = scales::percent_format()) +
  facet_wrap(~ version) +
  labs(title = paste0("User feedback"), 
       subtitle = "Feedback on user friendliness",
       caption = "Data source: TeamBaby") +
  xlab("Likert scale rating") + 
  ylab("Proportion") + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"), 
        plot.title = element_text(color = "#2F2E41", size = 12, face = "bold"),
        plot.subtitle = element_text(color = "#454543"),
        plot.caption = element_text(color = "#454543", face = "italic")
  )

# content

ggplot(data = data_feedback) + 
  geom_bar(mapping = aes(x = content, 
                         y = ..prop.., 
                         group = version), 
           stat = "count", 
           fill = "#454543") + 
  scale_y_continuous(labels = scales::percent_format()) +
  facet_wrap(~ version) +
  labs(title = paste0("User feedback"), 
       subtitle = "Feedback on content",
       caption = "Data source: TeamBaby") +
  xlab("Likert scale rating") + 
  ylab("Proportion") + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"), 
        plot.title = element_text(color = "#2F2E41", size = 12, face = "bold"),
        plot.subtitle = element_text(color = "#454543"),
        plot.caption = element_text(color = "#454543", face = "italic")
  )

# utility

ggplot(data = data_feedback) + 
  geom_bar(mapping = aes(x = utility, 
                         y = ..prop.., 
                         group = version), 
           stat = "count", 
           fill = "#454543") + 
  scale_y_continuous(labels = scales::percent_format()) +
  facet_wrap(~ version) +
  labs(title = paste0("User feedback"), 
       subtitle = "Feedback on utility",
       caption = "Data source: TeamBaby") +
  xlab("Likert scale rating") + 
  ylab("Proportion") + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"), 
        plot.title = element_text(color = "#2F2E41", size = 12, face = "bold"),
        plot.subtitle = element_text(color = "#454543"),
        plot.caption = element_text(color = "#454543", face = "italic")
  )

## ---- age-plot

age_responses <- app_v1_baseline_subset[!is.na(app_v1_baseline_subset$Alter), ]

ggplot(data = age_responses, aes(x = Alter)) + 
  geom_histogram(binwidth = 1, fill = "#2F2E41") +
  labs(title = paste0("Demographics of Version 1 users"), 
       subtitle = "Histogram of age distribution",
       caption = "Data source: TeamBaby") +
  xlab("Age") + 
  ylab("Frequency") + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"), 
        plot.title = element_text(color = "#2F2E41", size = 12, face = "bold"),
        plot.subtitle = element_text(color = "#454543"),
        plot.caption = element_text(color = "#454543", face = "italic")
  )

## ---- education-plot

education_responses <- app_v1_baseline_subset[!is.na(app_v1_baseline_subset$Ausbildung), ]

ggplot(data = education_responses, aes(x = factor(Ausbildung))) + 
  geom_bar(stat = "count", 
           fill = "#454543") + 
  labs(title = paste0("Demographics of Version 1 users"), 
       subtitle = "Bar chart of education attainment",
       caption = "Data source: TeamBaby") +
  xlab("Education") + 
  ylab("Frequency") + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"), 
        plot.title = element_text(color = "#2F2E41", size = 12, face = "bold"),
        plot.subtitle = element_text(color = "#454543"),
        plot.caption = element_text(color = "#454543", face = "italic")
  )

## ---- education-plot

family_responses <- app_v1_baseline_subset[!is.na(app_v1_baseline_subset$Familie), ]

ggplot(data = family_responses, aes(x = factor(Familie))) + 
  geom_bar(stat = "count", 
           fill = "#454543") + 
  labs(title = paste0("Demographics of Version 1 users"), 
       subtitle = "Bar chart of marital status",
       caption = "Data source: TeamBaby") +
  xlab("Education") + 
  ylab("Frequency") + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"), 
        plot.title = element_text(color = "#2F2E41", size = 12, face = "bold"),
        plot.subtitle = element_text(color = "#454543"),
        plot.caption = element_text(color = "#454543", face = "italic")
  )

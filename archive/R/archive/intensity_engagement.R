## ---- pages-viewed-plot

pages <- final_data[!duplicated(final_data[ ,c('UserCode')]),]

ggplot(pages, aes(x = pages_viewed)) + 
  geom_histogram(binwidth=10, fill = "#2F2E41") +
  labs(title = "Intensity of engagement", 
       subtitle = "Histogram of pages viewed per participant",
       caption = "Data source: TeamBaby") +
  xlab("Pages viewed") + 
  ylab("Frequency") + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"), 
        plot.title = element_text(color = "#2F2E41", size = 12, face = "bold"),
        plot.subtitle = element_text(color = "#454543", face = "bold"),
        plot.caption = element_text(color = "#454543", face = "italic")
  )

## ---- pages-viewed-plot

lessons <- final_data[!duplicated(final_data[ ,c('UserCode')]),]

ggplot(lessons, aes(lessons_viewed)) + 
  geom_bar(aes(y = (..count..)/sum(..count..)), fill = "#454543") + 
  labs(title = paste0("Intensity of engagement"), 
       subtitle = "Bar chart of lessons completed per user",
       caption = "Data source: TeamBaby") +
  xlab("Lessons completed") + 
  ylab("Proportion") + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"), 
        plot.title = element_text(color = "#2F2E41", size = 12, face = "bold"),
        plot.subtitle = element_text(color = "#454543"),
        plot.caption = element_text(color = "#454543", face = "italic")
  )

## ---- open-ended-items

open_items <- final_data[!duplicated(final_data[ ,c('UserCode')]),]

ggplot(open_items, aes(total_open_items)) + 
  geom_histogram(binwidth=10, fill = "#2F2E41") +
  labs(title = "Intensity of engagement", 
       subtitle = "Histogram of open items completed per participant",
       caption = "Data source: TeamBaby") +
  xlab("Open ended items completed") + 
  ylab("Frequency") + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"), 
        plot.title = element_text(color = "#2F2E41", size = 12, face = "bold"),
        plot.subtitle = element_text(color = "#454543", face = "bold"),
        plot.caption = element_text(color = "#454543", face = "italic")
  )

## ---- action-plan-created

action_plan <- final_data[!duplicated(final_data[ ,c('UserCode')]),]

ggplot(action_plan, aes(action_plan)) + 
  geom_bar(aes(y = (..count..)/sum(..count..)), fill = "#454543") + 
  labs(title = paste0("Intensity of engagement"), 
       subtitle = "Bar chart of lessons completed per user",
       caption = "Data source: TeamBaby") +
  xlab("Action plan completed") + 
  ylab("Proportion") + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"), 
        plot.title = element_text(color = "#2F2E41", size = 12, face = "bold"),
        plot.subtitle = element_text(color = "#454543"),
        plot.caption = element_text(color = "#454543", face = "italic")
  )

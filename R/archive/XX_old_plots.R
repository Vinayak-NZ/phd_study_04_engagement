## ---- age-plot

app_v1_baseline_valid <- app_v1_baseline[app_v1_baseline$Alter > 18, ] 

app_v1_baseline_valid_age <- app_v1_baseline_valid[!is.na(app_v1_baseline_valid$Alter), ]

ggplot(app_v1_baseline_valid_age, aes(x = Alter)) +
  geom_histogram(bins = 15, fill = "#4739a2") +
  labs(title = paste0("Distribution of age"), 
       subtitle = "Histogram of age of participants in study",
       caption = "Data source: TeamBaby") +
  xlab("Age") + 
  ylab("Count") + 
  xlim(0, 60) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"), 
        plot.title = element_text(color = "#2F2E41", size = 12, face = "bold"),
        plot.subtitle = element_text(color = "#454543"),
        plot.caption = element_text(color = "#454543", face = "italic")
  )

## ---- education-plot

app_v1_baseline_valid_education <- 
  app_v1_baseline[app_v1_baseline$Ausbildung %in% 
                    c(0, 1, 2, 3, 4, 5, 6, 7), ]

app_v1_baseline_valid_education$Ausbildung <- 
  ifelse(app_v1_baseline_valid_education$Ausbildung %in% c(0,1), 
         1, 
         app_v1_baseline_valid_education$Ausbildung)

app_v1_baseline_valid_education$Ausbildung <- 
  ifelse(app_v1_baseline_valid_education$Ausbildung == 1, 
         "L1", 
         ifelse(app_v1_baseline_valid_education$Ausbildung == 2, 
                "L2", 
                ifelse(app_v1_baseline_valid_education$Ausbildung == 3, 
                       "L3", 
                       ifelse(app_v1_baseline_valid_education$Ausbildung == 4, 
                              "L4", 
                              ifelse(app_v1_baseline_valid_education$Ausbildung == 5, 
                                     "L5", 
                                     ifelse(app_v1_baseline_valid_education$Ausbildung == 6, 
                                            "L6", "L7"))))))

labels <- c(L1 = 'L1 = No school-leaving qualification', 
            L2 = 'L2 = Secondary/ elementary school', 
            L3 = 'L3 = Secondary school diploma', 
            L4 = 'L4 = A-levels', 
            L5 = 'L5 = Completed vocational training', 
            L6 = 'L6 = University degree - Hochschule', 
            L7 = 'L7 = University degree')

ggplot(data = app_v1_baseline_valid_education, aes(x = Ausbildung, fill = Ausbildung)) + 
  geom_bar(key_glyph = draw_key_blank) + 
  scale_fill_manual(values = rep("#4739a2", length(labels)), labels = labels) +
  labs(title = paste0("Education level"), 
       subtitle = "Bar chart of highest qualification obtained",
       caption = "Data source: TeamBaby", 
       fill = "Education") +
  xlab("Education") + 
  ylab("Count") + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"), 
        plot.title = element_text(color = "#2F2E41", size = 12, face = "bold"),
        plot.subtitle = element_text(color = "#454543"),
        plot.caption = element_text(color = "#454543", face = "italic"), 
        legend.key = element_rect(fill = NA),
        legend.key.width = unit(0, "pt"),
        legend.spacing.x = unit(0, "pt")) 

## ---- family-comp

app_v1_baseline_valid_family <- 
  app_v1_baseline[app_v1_baseline$Familie %in% 
                    c(0, 1, 2, 3, 4, 5), ]

app_v1_baseline_valid_family$Familie <- 
  ifelse(app_v1_baseline_valid_family$Familie %in% c(0,1), 
         "RS1", 
         ifelse(app_v1_baseline_valid_family$Familie == 2, 
                "RS2", 
                ifelse(app_v1_baseline_valid_family$Familie == 3, 
                       "RS3", "RS4")))

level_order <- c("RS1", 
                 "RS2", 
                 "RS3", 
                 "RS4")

labels <- c(RS1 = 'RS1 = Single', 
            RS2 = 'RS2 = Committed relationship and not registered', 
            RS3 = 'RS3 = Married/ Registered', 
            RS4 = 'RS4 = Divorced')

ggplot(data = app_v1_baseline_valid_family, aes(x = Familie, fill = Familie)) + 
  geom_bar(key_glyph = draw_key_blank) + 
  scale_fill_manual(values = rep("#4739a2", length(labels)), labels = labels) +
  labs(title = paste0("Relationship status"), 
       subtitle = "Bar chart of participant relationship status",
       caption = "Data source: TeamBaby", 
       fill = "Relationship status") +
  xlab("Relatonship status") + 
  ylab("Count") + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"), 
        plot.title = element_text(color = "#2F2E41", size = 12, face = "bold"),
        plot.subtitle = element_text(color = "#454543"),
        plot.caption = element_text(color = "#454543", face = "italic"), 
        legend.key = element_rect(fill = NA),
        legend.key.width = unit(0, "pt"),
        legend.spacing.x = unit(0, "pt")
  ) 


## ---- age-plot

app_baseline_valid <- app_baseline_demo[app_baseline_demo$alter > 18, ]

app_baseline_valid_age <- 
  app_baseline_valid[!is.na(app_baseline_valid$alter), ]

ggplot(app_baseline_valid_age, aes(x = alter, fill = version)) +
  geom_density(alpha=0.6, position = 'identity') +
  scale_fill_manual(values=c("#46e7fd", "#e18b22")) +
  labs(title = paste0("Distribution of age"), 
       subtitle = "Histogram of age of participants in study",
       caption = "Data source: TeamBaby", 
       fill = "Web-app version") +
  xlab("Age") + 
  ylab("Density") + 
  xlim(0, 60) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"), 
        plot.title = element_text(color = "#2F2E41", size = 12, face = "bold"),
        plot.subtitle = element_text(color = "#454543"),
        plot.caption = element_text(color = "#454543", face = "italic")
  )

## ---- education-plot

app_baseline_valid_education <- 
  app_baseline_demo[app_baseline_demo$ausbildung %in% 
                 c(0, 1, 2, 3, 4, 5, 6, 7), ]

app_baseline_valid_education$ausbildung <- 
  ifelse(app_baseline_valid_education$ausbildung %in% c(0,1), 
         1, 
         app_baseline_valid_education$ausbildung)

app_baseline_valid_education$ausbildung <- 
  ifelse(app_baseline_valid_education$ausbildung == 1, 
         "L1", 
         ifelse(app_baseline_valid_education$ausbildung == 2, 
                "L2", 
                ifelse(app_baseline_valid_education$ausbildung == 3, 
                       "L3", 
                       ifelse(app_baseline_valid_education$ausbildung == 4, 
                              "L4", 
                              ifelse(app_baseline_valid_education$ausbildung == 5, 
                                     "L5", 
                                     ifelse(app_baseline_valid_education$ausbildung == 6, 
                                            "L6", "L7"))))))

labels.ed <- c(L1 = 'L1 = No school-leaving qualification', 
               L2 = 'L2 = Secondary/ elementary school', 
               L3 = 'L3 = Secondary school diploma', 
               L4 = 'L4 = A-levels', 
               L5 = 'L5 = Completed vocational training', 
               L6 = 'L6 = University degree - Hochschule', 
               L7 = 'L7 = University degree')

table_education <- as.data.frame(
  table(app_baseline_valid_education$version, 
        app_baseline_valid_education$ausbildung))

names(table_education)[names(table_education) == "Var1"] <- 
  "version"

names(table_education)[names(table_education) == "Var2"] <- 
  "ausbildung"

names(table_education)[names(table_education) == "Freq"] <- 
  "frequency"

ggplot(table_education, 
       aes(fill = ausbildung, 
           y = frequency, 
           x = version)) + 
  geom_bar(position="fill", stat="identity") + 
  scale_fill_manual(values = c("#000", 
                               "#46e7fd", 
                               "#4739a2", 
                               "#e18b22", 
                               "#fefe62", 
                               "#d35fb7", 
                               "#009e73"), 
                    labels = labels.ed) +
  labs(title = paste0("Education level"), 
       subtitle = "Bar chart of highest qualification obtained",
       caption = "Data source: TeamBaby", 
       fill = "Education") +
  xlab("Education") + 
  ylab("Proportion") + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"), 
        plot.title = element_text(color = "#2F2E41", size = 12, face = "bold"),
        plot.subtitle = element_text(color = "#454543"),
        plot.caption = element_text(color = "#454543", face = "italic")) 

## ---- family-comp

app_baseline_valid_family <- 
  app_baseline_demo[app_baseline_demo$familie %in% 
                 c(0, 1, 2, 3, 4, 5), ]

app_baseline_valid_family$familie <- 
  ifelse(app_baseline_valid_family$familie %in% c(0,1), 
         "RS1", 
         ifelse(app_baseline_valid_family$familie == 2, 
                "RS2", 
                ifelse(app_baseline_valid_family$familie == 3, 
                       "RS3", "RS4")))

level_order <- c("RS1", 
                 "RS2", 
                 "RS3", 
                 "RS4")

labels.fm <- c(RS1 = 'RS1 = Single', 
               RS2 = 'RS2 = Committed relationship and not registered', 
               RS3 = 'RS3 = Married/ Registered', 
               RS4 = 'RS4 = Divorced')

table_family <- as.data.frame(
  table(app_baseline_valid_family$version, 
        app_baseline_valid_family$familie))

names(table_family)[names(table_family) == "Var1"] <- 
  "version"

names(table_family)[names(table_family) == "Var2"] <- 
  "familie"

names(table_family)[names(table_family) == "Freq"] <- 
  "frequency"

ggplot(table_family, 
       aes(fill = familie, 
           y = frequency, 
           x = version)) + 
  geom_bar(position="fill", stat="identity") + 
  scale_fill_manual(values = c("#000", 
                               "#46e7fd", 
                               "#4739a2", 
                               "#e18b22"), 
                    labels = labels.fm) +
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
        plot.caption = element_text(color = "#454543", face = "italic")
  ) 


## ---- age-in-app

data_no_impute_age <- 
  data_no_impute[!is.na(data_no_impute$in_app_age_grp), ]

data_no_impute_age$version <- 
  ifelse(data_no_impute_age$co_creation_method == 0, 1, 2)

data_no_impute_age$in_app_age_grp <- 
  ifelse(data_no_impute_age$in_app_age_grp == 1, 
         "< 25", 
         ifelse(data_no_impute_age$in_app_age_grp == 2, 
                "25 - 40", 
                ifelse(data_no_impute_age$in_app_age_grp == 3, 
                       "40 - 55", NA)))

table_in_app_age <- as.data.frame(
  table(data_no_impute_age$version, 
        data_no_impute_age$in_app_age_grp))

names(table_in_app_age)[names(table_in_app_age) == "Var1"] <- 
  "version"

names(table_in_app_age)[names(table_in_app_age) == "Var2"] <- 
  "age"

names(table_in_app_age)[names(table_in_app_age) == "Freq"] <- 
  "frequency"

ggplot(table_in_app_age, 
       aes(fill = age, 
           y = frequency, 
           x = version)) + 
  geom_bar(position="fill", stat="identity") + 
  scale_fill_manual(values = c("#46e7fd", 
                               "#4739a2", 
                               "#e18b22")) +
  labs(title = paste0("Age breakdowns of end users"), 
       subtitle = "Bar chart of age of end users",
       fill = "Age") +
  xlab("Version") + 
  ylab("Proportion") + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"), 
        plot.title = element_text(color = "#2F2E41", size = 12, face = "bold"),
        plot.subtitle = element_text(color = "#454543"),
        plot.caption = element_text(color = "#454543", face = "italic"))



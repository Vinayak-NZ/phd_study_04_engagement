## ---- output-hapa-profile

hapa_df <- tibble(
  construct = c("Risk perception", 
                "Outcome expectancy", 
                "Communication self-efficacy", 
                "Intention", 
                "Planning"),
  v1 = c(median(data_no_impute[data_no_impute$co_creation_method==0, ]$hapa1_t1, na.rm = TRUE), 
         median(data_no_impute[data_no_impute$co_creation_method==0, ]$hapa2_t1, na.rm = TRUE), 
         median(data_no_impute[data_no_impute$co_creation_method==0, ]$hapa3_t1, na.rm = TRUE), 
         median(data_no_impute[data_no_impute$co_creation_method==0, ]$hapa4_t1, na.rm = TRUE), 
         median(data_no_impute[data_no_impute$co_creation_method==0, ]$hapa5_t1, na.rm = TRUE)),
  v2 = c(median(data_no_impute[data_no_impute$co_creation_method==1, ]$hapa1_t1, na.rm = TRUE), 
         median(data_no_impute[data_no_impute$co_creation_method==1, ]$hapa2_t1, na.rm = TRUE), 
         median(data_no_impute[data_no_impute$co_creation_method==1, ]$hapa3_t1, na.rm = TRUE), 
         median(data_no_impute[data_no_impute$co_creation_method==1, ]$hapa4_t1, na.rm = TRUE), 
         median(data_no_impute[data_no_impute$co_creation_method==1, ]$hapa5_t1, na.rm = TRUE))
)

hapa_df$construct <- 
  factor(hapa_df$construct, levels = c("Risk perception", 
                                          "Outcome expectancy", 
                                          "Communication self-efficacy", 
                                          "Intention", 
                                          "Planning"))

hapa_plot_df <- bind_rows(
  hapa_df %>% mutate(version = "Version 1", score = v1),
  hapa_df %>% mutate(version = "Version 2", score = v2)
)

plot_hapa <- 
  ggplot(hapa_plot_df) +
  geom_segment(data = hapa_df,
               aes(x = v1, xend = v2, 
                   y = construct, 
                   yend = construct),
               colour = "grey70", 
               linewidth = 1.5) +
  geom_point(aes(x = score, 
                 y = construct, 
                 colour = version), size = 7) +
  scale_colour_manual(values = c("Version 1" = "#1B4F72",
                                 "Version 2" = "#A93226")) +
  labs(
    title = "A. Baseline HAPA Motivational Profiles",
    x = "Median Score (0–100)",
    y = NULL,
    colour = "Version"
  ) +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    axis.line = element_line(colour = "black"),
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 14),
    legend.text = element_text(size = 14),
    legend.title = element_text(size = 14),
    plot.title = element_text(color = "#2F2E41", size = 12, face = "bold")
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

plot_age <- 
  ggplot(table_in_app_age, 
         aes(fill = age, 
             y = frequency, 
             x = version)) + 
  geom_bar(position="fill", stat="identity") + 
  scale_fill_manual(values = c("#46e7fd", 
                               "#4739a2", 
                               "#e18b22")) + 
  scale_x_discrete(labels = c("1" = "Version 1",
                              "2" = "Version 2")) +
  labs(title = "B. Age Distribution of V1 and V2 Users (in proportions)", 
       y = "Proportion",
       x = "",
       fill = "Age") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"), 
        plot.title = element_text(color = "#2F2E41", size = 12, face = "bold"),
        plot.subtitle = element_text(color = "#454543"),
        plot.caption = element_text(color = "#454543", face = "italic")) + 
  coord_flip()

combined_plot <- plot_hapa / plot_age +
  plot_annotation(
    title = "End User Profile for V1 and V2",
    subtitle = "Baseline motivational profiles (HAPA) and age distribution at sign-in",
    theme = theme(
      plot.title = element_text(size = 18, face = "bold"),
      plot.subtitle = element_text(size = 14)
    )
  ) 

ggsave("output/combined_plot.png", 
       plot = combined_plot, 
       device = 'tiff', 
       width = 12, height = 6)

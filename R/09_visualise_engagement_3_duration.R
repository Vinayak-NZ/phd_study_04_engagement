## ---- time-spent-per-visit

time_visit <- final_data_key_vars[
  !duplicated(final_data_key_vars[ , c('version',
                                       'UserCode', 
                                       'log_in_count')]),]

ggplot(time_visit, aes(x = time_spent_visit, fill = version)) +
  geom_density(alpha = 0.7) + 
  scale_fill_manual(values = c("#46e7fd", "#e18b22")) + 
  labs(title = "Duration of engagement", 
       subtitle = "Distribution of time spent per visit",
       caption = "Data source: TeamBaby") +
  xlab("Time spent per visit (minutes)") + 
  ylab("Density") + 
  guides(fill = guide_legend(title = "Web-app Version")) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"), 
        plot.title = element_text(color = "#2F2E41", size = 12, face = "bold"),
        plot.subtitle = element_text(color = "#454543", face = "bold"),
        plot.caption = element_text(color = "#454543", face = "italic")
  )

## ---- time-spent-test
t_test_time_spent <- t.test(time_spent_visit ~ version, 
       time_visit, 
       var.equal=TRUE)

time_visit$time_spent_visit <- as.numeric(time_visit$time_spent_visit)

t_test_time_spent_effect_size <- 
  cohens_d(time_visit, 
           time_spent_visit ~ version, 
           var.equal = TRUE)

time_spent_table <- data.frame(V1_mean = t_test_time_spent$estimate[[1]], 
                          V2_mean = t_test_time_spent$estimate[[2]], 
                          df = t_test_time_spent$parameter[[1]], 
                          test_statistic = t_test_time_spent$statistic[[1]], 
                          p_value = ifelse(t_test_time_spent$p.value[[1]] < 0.001, "< 0.001", 
                                           ifelse(t_test_time_spent$p.value[[1]] < 0.05, "< 0.05", "not sig.")))

kbl(time_spent_table) %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed")) %>%
  add_header_above(c("Average time spent (per log-in and user)" = 2, "Independent t-test" = 3))


## ---- days-between

days_data <- final_data[
  !duplicated(final_data[ , c(
    'version', 
    'UserCode')]),]

days_data$days_group <- ifelse(days_data$days_between == 0, "1 day", 
                               ifelse(
                                 days_data$days_between > 0 & 
                                   days_data$days_between <= 7, "1 week", 
                                 ifelse(
                                   days_data$days_between > 8 & 
                                     days_data$days_between < 31, "1 month",
                                   "Month +")))

level_order <- c('1 day', '1 week', '1 month', 'Month +') 

ggplot(data = days_data) + 
  geom_bar(mapping = aes(x = factor(days_group, levels = level_order), 
                         y = ..prop.., 
                         group = version), 
           stat = "count", 
           fill = "#4739a2") + 
  scale_y_continuous(labels = scales::percent_format()) +
  facet_wrap(~ version) +
  labs(title = paste0("Frequency of engagement"), 
       subtitle = "Bar chart of duration of use",
       caption = "Data source: TeamBaby") +
  xlab("Duration of use") + 
  ylab("Proportion") + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"), 
        plot.title = element_text(color = "#2F2E41", size = 12, face = "bold"),
        plot.subtitle = element_text(color = "#454543"),
        plot.caption = element_text(color = "#454543", face = "italic")
  )

## ---- days-between-test
t_test_days_between <- t.test(days_between ~ version, 
       days_data, 
       var.equal=TRUE)

days_data$days_between <- as.numeric(days_data$days_between)

t_test_days_between_effect_size <- 
  cohens_d(days_data, 
           days_between ~ version, 
           var.equal = TRUE)

days_between_table <- data.frame(V1_mean = t_test_days_between$estimate[[1]], 
                               V2_mean = t_test_days_between$estimate[[2]], 
                               df = t_test_days_between$parameter[[1]], 
                               test_statistic = t_test_days_between$statistic[[1]], 
                               p_value = ifelse(t_test_days_between$p.value[[1]] < 0.001, "< 0.001", 
                                                ifelse(t_test_days_between$p.value[[1]] < 0.05, "< 0.05", "not sig.")))

kbl(days_between_table) %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed")) %>%
  add_header_above(c("Average duration of use (days per user)" = 2, "Independent t-test" = 3))


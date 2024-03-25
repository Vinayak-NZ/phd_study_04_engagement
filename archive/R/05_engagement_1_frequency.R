## ---- average-log-ins-plot

data_log_in <- final_data_key_vars[
  !duplicated(final_data_key_vars[ ,c('version', 
                                      'UserCode')]),]

data_log_in$log_in_count_group <- ifelse(data_log_in$total_log_in == 1, 1, 
                                         ifelse(
                                           data_log_in$total_log_in > 1 & 
                                             data_log_in$total_log_in < 6, "2 to 5", 
                                           "6+"))

ggplot(data = data_log_in) + 
  geom_bar(mapping = aes(x = log_in_count_group, 
                         y = ..prop.., 
                         group = version), 
           stat = "count", 
           fill = "#454543") + 
  scale_y_continuous(labels = scales::percent_format()) +
  facet_wrap(~ version) +
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

## ---- log-ins-test
t_test_log_in <- t.test(total_log_in ~ version, 
                        data_log_in, 
                        var.equal=TRUE)

log_in_table <- data.frame(`Version 1` = t_test_log_in$estimate[[1]], 
                           `Version 2` = t_test_log_in$estimate[[2]], 
                           df = t_test_log_in$parameter[[1]], 
                           `Test-statistic` = t_test_log_in$statistic[[1]], 
                           `p-value` = ifelse(t_test_log_in$p.value[[1]] < 0.001, "< 0.001", 
                           ifelse(t_test_log_in$p.value[[1]] < 0.05, "< 0.05", "not sig.")))

kbl(log_in_table) %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed")) %>%
  add_header_above(c("Average Log ins (per user)" = 2, "Independent t-test" = 3))

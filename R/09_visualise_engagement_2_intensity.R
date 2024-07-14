## ---- pages-viewed-plot

pages <- final_data_key_vars[!duplicated(final_data_key_vars[ ,c('version', 
                                               'UserCode')]),]

pages$proption_viewed <- ifelse(pages$version == "Version 1", 
                                (pages$pages_viewed/141)*100,
                                (pages$pages_viewed/112)*100)

ggplot(pages, aes(x = proption_viewed, fill = version)) +
  geom_density(alpha = 0.7) + 
  scale_fill_manual(values = c("#46e7fd", "#e18b22")) + 
  labs(title = "Intensity of engagement", 
       subtitle = "Distribution of pages viewed per participant",
       caption = "Data source: TeamBaby") +
  xlab("Proportion of pages viewed") + 
  ylab("Density") + 
  guides(fill = guide_legend(title = "Web-app Version")) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"), 
        plot.title = element_text(color = "#2F2E41", size = 12, face = "bold"),
        plot.subtitle = element_text(color = "#454543", face = "bold"),
        plot.caption = element_text(color = "#454543", face = "italic")
  )

## ---- pages-viewed-test
t_test_log_in_pages <- t.test(proption_viewed ~ version, 
       pages, 
       var.equal=TRUE)

t_test_log_in_pages_effect_size <- 
  cohens_d(pages, 
           proption_viewed ~ version, 
           var.equal = TRUE)

pages_table <- data.frame(`Version 1` = t_test_log_in_pages$estimate[[1]], 
                           `Version 2` = t_test_log_in_pages$estimate[[2]], 
                           df = t_test_log_in_pages$parameter[[1]], 
                           `Test-statistic` = t_test_log_in_pages$statistic[[1]], 
                          `p-value` = ifelse(t_test_log_in_pages$p.value[[1]] < 0.001, "< 0.001", 
                                           ifelse(t_test_log_in_pages$p.value[[1]] < 0.05, "< 0.05", "not sig.")))

kbl(pages_table) %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed")) %>%
  add_header_above(c("Average pages viewed (proportion per user)" = 2, "Independent t-test" = 3))

## ---- lessons-viewed-plot

lessons <- final_data_key_vars[
  !duplicated(final_data_key_vars[ , c('version',
                                       'UserCode')]),]

ggplot(data = lessons) + 
  geom_bar(mapping = aes(x = lessons_viewed, 
                         y = ..prop.., 
                         group = version), 
           stat = "count", 
           fill = "#4739a2") + 
  scale_y_continuous(labels = scales::percent_format()) +
  facet_wrap(~ version) +
  labs(title = paste0("Intensity of engagement"), 
       subtitle = "Bar chart of lessons completed",
       caption = "Data source: TeamBaby") +
  xlab("Max lessons completed") + 
  ylab("Proportion") + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"), 
        plot.title = element_text(color = "#2F2E41", size = 12, face = "bold"),
        plot.subtitle = element_text(color = "#454543"),
        plot.caption = element_text(color = "#454543", face = "italic")
  )

## ---- lessons-viewed-test
t_test_lessons_viewed <- t.test(lessons_viewed ~ version, 
       lessons, 
       var.equal=TRUE)

t_test_lessons_effect_size <- 
  cohens_d(lessons, 
           lessons_viewed ~ version, 
           var.equal = TRUE)

lessons_viewed_table <- data.frame(`Version 1` = t_test_lessons_viewed$estimate[[1]], 
                          `Version 2` = t_test_lessons_viewed$estimate[[2]], 
                          df = t_test_lessons_viewed$parameter[[1]], 
                          `Test-statistic` = t_test_lessons_viewed$statistic[[1]], 
                          `p-value` = ifelse(t_test_lessons_viewed$p.value[[1]] < 0.001, "< 0.001", 
                                           ifelse(t_test_lessons_viewed$p.value[[1]] < 0.05, "< 0.05", "not sig.")))

kbl(lessons_viewed_table) %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed")) %>%
  add_header_above(c("Average lessons viewed (per user)" = 2, "Independent t-test" = 3))


## ---- open-ended-items-plot

open_items <- final_data_key_vars[
  !duplicated(final_data_key_vars[ , c('version', 
                              'UserCode')]),]

open_items$proportion_completed <- ifelse(open_items$version == "Version 1", 
                                (open_items$total_open_items/91)*100,
                                (open_items$total_open_items/42)*100)

ggplot(open_items, aes(x = proportion_completed, fill = version)) +
  geom_density(alpha = 0.7) + 
  scale_fill_manual(values = c("#46e7fd", "#e18b22")) + 
  labs(title = "Intensity of engagement", 
       subtitle = "Distribution of open ended items completed per participant",
       caption = "Data source: TeamBaby") +
  xlab("Proportion of open ended items completed") + 
  ylab("Density") + 
  guides(fill = guide_legend(title = "Web-app Version")) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"), 
        plot.title = element_text(color = "#2F2E41", size = 12, face = "bold"),
        plot.subtitle = element_text(color = "#454543", face = "bold"),
        plot.caption = element_text(color = "#454543", face = "italic")
  )

## ---- open-items-test
t_test_open_items_viewed <- t.test(proportion_completed ~ version, 
       open_items, 
       var.equal=TRUE)

t_test_open_items_effect_size <- 
  cohens_d(open_items, 
           proportion_completed ~ version, 
           var.equal = TRUE)

open_items_table <- data.frame(`Version 1` = t_test_open_items_viewed$estimate[[1]], 
                                   `Version 2` = t_test_open_items_viewed$estimate[[2]], 
                                   df = t_test_open_items_viewed$parameter[[1]], 
                                   `Test-statistic` = t_test_open_items_viewed$statistic[[1]], 
                               `p-value` = ifelse(t_test_open_items_viewed$p.value[[1]] < 0.001, "< 0.001", 
                                                ifelse(t_test_open_items_viewed$p.value[[1]] < 0.05, "< 0.05", "not sig.")))

kbl(open_items_table) %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed")) %>%
  add_header_above(c("Average open-ended items completed (proportion per user)" = 2, "Independent t-test" = 3))


## ---- action-plan-created-plot

action_plan <- final_data_key_vars[
  !duplicated(final_data_key_vars[ , c('version', 
                              'UserCode')]),]

action_plan$action_plan <- ifelse(action_plan$action_plan == 0, "No", "Yes")

ggplot(data = action_plan) + 
  geom_bar(mapping = aes(x = action_plan, 
                         y = after_stat(prop), 
                         group = version), 
           stat = "count", 
           fill = "#4739a2") + 
  scale_y_continuous(labels = scales::percent_format()) +
  facet_wrap(~ version) +
  labs(title = paste0("Intensity of engagement"), 
       subtitle = "Bar chart of action plans completed",
       caption = "Data source: TeamBaby") +
  xlab("Action plans completed") + 
  ylab("Proportion") + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"), 
        plot.title = element_text(color = "#2F2E41", size = 12, face = "bold"),
        plot.subtitle = element_text(color = "#454543"),
        plot.caption = element_text(color = "#454543", face = "italic")
  )

## ---- action-plans-test
chisq_action_plan <- chisq.test(action_plan$version, 
           action_plan$action_plan, 
           correct=FALSE)

t_test_open_items_effect_size <- 
  cohens_d(open_items, 
           proportion_completed ~ version, 
           var.equal = TRUE)

action_plan_table_count <- table(action_plan$version, 
                                 action_plan$action_plan)

action_chi_square_df <- min(dim(action_plan_table_count)) - 1

action_plan_effect_size <- cramers_V(chi = chisq_action_plan$statistic, 
                                     n = sum(action_plan_table_count), 
                                     df = action_chi_square_df)

action_plan_table <- data.frame(`Version 1` = (chisq_action_plan$observed[[3]])/(chisq_action_plan$observed[[1]] + chisq_action_plan$observed[[3]]) * 100, 
                               `Version 2` = (chisq_action_plan$observed[[4]])/(chisq_action_plan$observed[[2]] + chisq_action_plan$observed[[4]]) * 100, 
                               df = chisq_action_plan$parameter[[1]], 
                               `Test-statistic` = chisq_action_plan$statistic[[1]], 
                               `p-value` = ifelse(chisq_action_plan$p.value[[1]] < 0.001, "< 0.001", 
                                                ifelse(chisq_action_plan$p.value[[1]] < 0.05, "< 0.05", "not sig.")))

kbl(action_plan_table) %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed")) %>%
  add_header_above(c("Action plan completed (proportion of users)" = 2, "Independent t-test" = 3))


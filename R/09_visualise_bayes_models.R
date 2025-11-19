## ---- plot-model-estimates

# model-freq
model_freq_summary <- 
  broom.mixed::tidy(model_freq, effects = "fixed", conf.int = TRUE)

model_freq_summary$term <- dplyr::recode(model_freq_summary$term,
                                    "(Intercept)" = "Intercept",
                                    "co_creation_method1" = "Co-creation methods",
                                    "in_app_age_grp.L" = "Age (Linear)",
                                    "in_app_age_grp.Q" = "Age (Quadratic)"
)

model_freq_summary_plot <- 
  ggplot(model_freq_summary, 
         aes(x = estimate, 
             y = factor(term, 
                        levels = c("(Intercept)" = "Intercept", 
                                   "co_creation_method1" = "Co-creation methods", 
                                   "in_app_age_grp.L" = "Age (Linear)", 
                                   "in_app_age_grp.Q" = "Age (Quadratic)")))) +
  geom_point(color = "#205B87", size = 2) +
  geom_errorbar(aes(xmin = conf.low, xmax = conf.high), 
                 height = 0.2, color = "#205B87") +
  labs(title = "Frequency of Engagement - Pooled Posterior Estimates",
       subtitle = "95% Credible Intervals (from multiply imputed model)",
       x = "Estimate",
       y = NULL) +
  theme(
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(),
    panel.background = element_blank(), 
    axis.line = element_line(colour = "black"),
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 14),
    legend.text = element_text(size = 14),
    legend.title = element_text(size = 14),
    plot.title = element_text(color = "#2F2E41", size = 12, face = "bold"),
    plot.subtitle = element_text(color = "#454543"),
    plot.caption = element_text(color = "#454543", face = "italic")
  )

ggsave("output/model_freq_summary_plot.png", 
       plot = model_freq_summary_plot, 
       device = 'tiff', 
       width = 12, height = 6)

# model-progress
model_progress_summary <- 
  broom.mixed::tidy(model_progress, effects = "fixed", conf.int = TRUE)

model_progress_summary$term <- dplyr::recode(model_progress_summary$term,
                                         "(Intercept)" = "Intercept",
                                         "co_creation_method1" = "Co-creation methods",
                                         "in_app_age_grp.L" = "Age (Linear)",
                                         "in_app_age_grp.Q" = "Age (Quadratic)"
)

model_progress_summary_plot <- 
  ggplot(model_progress_summary, 
         aes(x = estimate, 
             y = factor(term, 
                        levels = c("(Intercept)" = "Intercept", 
                                   "co_creation_method1" = "Co-creation methods", 
                                   "in_app_age_grp.L" = "Age (Linear)", 
                                   "in_app_age_grp.Q" = "Age (Quadratic)")))) +
  geom_point(color = "#205B87", size = 2) +
  geom_errorbar(aes(xmin = conf.low, xmax = conf.high), 
                height = 0.2, color = "#205B87") +
  labs(title = "Frequency of Engagement - Pooled Posterior Estimates",
       subtitle = "95% Credible Intervals (from multiply imputed model)",
       x = "Estimate",
       y = NULL) +
  theme(
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(),
    panel.background = element_blank(), 
    axis.line = element_line(colour = "black"),
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 14),
    legend.text = element_text(size = 14),
    legend.title = element_text(size = 14),
    plot.title = element_text(color = "#2F2E41", size = 12, face = "bold"),
    plot.subtitle = element_text(color = "#454543"),
    plot.caption = element_text(color = "#454543", face = "italic")
  )

ggsave("output/model_progress_summary_plot.png", 
       plot = model_progress_summary_plot, 
       device = 'tiff', 
       width = 12, height = 6)

# model-items
model_items_summary <- 
  broom.mixed::tidy(model_items, effects = "fixed", conf.int = TRUE)

model_items_summary$term <- dplyr::recode(model_items_summary$term,
                                             "(Intercept)" = "Intercept",
                                             "co_creation_method1" = "Co-creation methods",
                                             "in_app_age_grp.L" = "Age (Linear)",
                                             "in_app_age_grp.Q" = "Age (Quadratic)"
)

model_items_summary_plot <- 
  ggplot(model_items_summary, 
         aes(x = estimate, 
             y = factor(term, 
                        levels = c("(Intercept)" = "Intercept", 
                                   "co_creation_method1" = "Co-creation methods", 
                                   "in_app_age_grp.L" = "Age (Linear)", 
                                   "in_app_age_grp.Q" = "Age (Quadratic)")))) +
  geom_point(color = "#205B87", size = 2) +
  geom_errorbar(aes(xmin = conf.low, xmax = conf.high), 
                height = 0.2, color = "#205B87") +
  labs(title = "Frequency of Engagement - Pooled Posterior Estimates",
       subtitle = "95% Credible Intervals (from multiply imputed model)",
       x = "Estimate",
       y = NULL) +
  theme(
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(),
    panel.background = element_blank(), 
    axis.line = element_line(colour = "black"),
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 14),
    legend.text = element_text(size = 14),
    legend.title = element_text(size = 14),
    plot.title = element_text(color = "#2F2E41", size = 12, face = "bold"),
    plot.subtitle = element_text(color = "#454543"),
    plot.caption = element_text(color = "#454543", face = "italic")
  )

ggsave("output/model_items_summary_plot.png", 
       plot = model_items_summary_plot, 
       device = 'tiff', 
       width = 12, height = 6)

# model-act-plan
model_act_plan_summary <- 
  broom.mixed::tidy(model_act_plan, effects = "fixed", conf.int = TRUE)

model_act_plan_summary$term <- dplyr::recode(model_act_plan_summary$term,
                                          "(Intercept)" = "Intercept",
                                          "co_creation_method1" = "Co-creation methods",
                                          "in_app_age_grp.L" = "Age (Linear)",
                                          "in_app_age_grp.Q" = "Age (Quadratic)"
)

model_act_plan_summary_plot <- 
  ggplot(model_act_plan_summary, 
         aes(x = estimate, 
             y = factor(term, 
                        levels = c("(Intercept)" = "Intercept", 
                                   "co_creation_method1" = "Co-creation methods", 
                                   "in_app_age_grp.L" = "Age (Linear)", 
                                   "in_app_age_grp.Q" = "Age (Quadratic)")))) +
  geom_point(color = "#205B87", size = 2) +
  geom_errorbar(aes(xmin = conf.low, xmax = conf.high), 
                height = 0.2, color = "#205B87") +
  labs(title = "Frequency of Engagement - Pooled Posterior Estimates",
       subtitle = "95% Credible Intervals (from multiply imputed model)",
       x = "Estimate",
       y = NULL) +
  theme(
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(),
    panel.background = element_blank(), 
    axis.line = element_line(colour = "black"),
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 14),
    legend.text = element_text(size = 14),
    legend.title = element_text(size = 14),
    plot.title = element_text(color = "#2F2E41", size = 12, face = "bold"),
    plot.subtitle = element_text(color = "#454543"),
    plot.caption = element_text(color = "#454543", face = "italic")
  )

ggsave("output/model_act_plan_summary_plot.png", 
       plot = model_act_plan_summary_plot, 
       device = 'tiff', 
       width = 12, height = 6)

# model-time-spent
model_time_spent_summary <- 
  broom.mixed::tidy(model_time_spent, effects = "fixed", conf.int = TRUE)

model_time_spent_summary$term <- dplyr::recode(model_time_spent_summary$term,
                                             "(Intercept)" = "Intercept",
                                             "co_creation_method1" = "Co-creation methods",
                                             "in_app_age_grp.L" = "Age (Linear)",
                                             "in_app_age_grp.Q" = "Age (Quadratic)"
)

model_time_spent_summary_plot <- 
  ggplot(model_time_spent_summary, 
         aes(x = estimate, 
             y = factor(term, 
                        levels = c("(Intercept)" = "Intercept", 
                                   "co_creation_method1" = "Co-creation methods", 
                                   "in_app_age_grp.L" = "Age (Linear)", 
                                   "in_app_age_grp.Q" = "Age (Quadratic)")))) +
  geom_point(color = "#205B87", size = 2) +
  geom_errorbar(aes(xmin = conf.low, xmax = conf.high), 
                height = 0.2, color = "#205B87") +
  labs(title = "Frequency of Engagement - Pooled Posterior Estimates",
       subtitle = "95% Credible Intervals (from multiply imputed model)",
       x = "Estimate",
       y = NULL) +
  theme(
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(),
    panel.background = element_blank(), 
    axis.line = element_line(colour = "black"),
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 14),
    legend.text = element_text(size = 14),
    legend.title = element_text(size = 14),
    plot.title = element_text(color = "#2F2E41", size = 12, face = "bold"),
    plot.subtitle = element_text(color = "#454543"),
    plot.caption = element_text(color = "#454543", face = "italic")
  )

ggsave("output/model_time_spent_summary_plot.png", 
       plot = model_time_spent_summary_plot, 
       device = 'tiff', 
       width = 12, height = 6)

# model-days-between
model_days_between_summary <- 
  broom.mixed::tidy(model_days_between, effects = "fixed", conf.int = TRUE)

model_days_between_summary$term <- dplyr::recode(model_days_between_summary$term,
                                               "(Intercept)" = "Intercept",
                                               "co_creation_method1" = "Co-creation methods",
                                               "in_app_age_grp.L" = "Age (Linear)",
                                               "in_app_age_grp.Q" = "Age (Quadratic)"
)

model_days_between_summary_plot <- 
  ggplot(model_days_between_summary, 
         aes(x = estimate, 
             y = factor(term, 
                        levels = c("(Intercept)" = "Intercept", 
                                   "co_creation_method1" = "Co-creation methods", 
                                   "in_app_age_grp.L" = "Age (Linear)", 
                                   "in_app_age_grp.Q" = "Age (Quadratic)")))) +
  geom_point(color = "#205B87", size = 2) +
  geom_errorbar(aes(xmin = conf.low, xmax = conf.high), 
                height = 0.2, color = "#205B87") +
  labs(title = "Frequency of Engagement - Pooled Posterior Estimates",
       subtitle = "95% Credible Intervals (from multiply imputed model)",
       x = "Estimate",
       y = NULL) +
  theme(
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(),
    panel.background = element_blank(), 
    axis.line = element_line(colour = "black"),
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 14),
    legend.text = element_text(size = 14),
    legend.title = element_text(size = 14),
    plot.title = element_text(color = "#2F2E41", size = 12, face = "bold"),
    plot.subtitle = element_text(color = "#454543"),
    plot.caption = element_text(color = "#454543", face = "italic")
  )

ggsave("output/model_days_between_summary_plot.png", 
       plot = model_days_between_summary_plot, 
       device = 'tiff', 
       width = 12, height = 6)

# model-comm
model_comm_summary <- 
  broom.mixed::tidy(model_comm, effects = "fixed", conf.int = TRUE)

model_comm_summary$term <- dplyr::recode(model_comm_summary$term,
                                                 "(Intercept)" = "Intercept",
                                                 "co_creation_method1" = "Co-creation methods",
                                                 "comm_mean_pre_scaled" = "Baseline scores"
)

model_comm_summary_plot <- 
  ggplot(model_comm_summary, 
         aes(x = estimate, 
             y = factor(term, 
                        levels = c("(Intercept)" = "Intercept", 
                                   "co_creation_method1" = "Co-creation methods", 
                                   "comm_mean_pre_scaled" = "Baseline scores")))) +
  geom_point(color = "#205B87", size = 2) +
  geom_errorbar(aes(xmin = conf.low, xmax = conf.high), 
                height = 0.2, color = "#205B87") +
  labs(title = "Frequency of Engagement - Pooled Posterior Estimates",
       subtitle = "95% Credible Intervals (from multiply imputed model)",
       x = "Estimate",
       y = NULL) +
  theme(
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(),
    panel.background = element_blank(), 
    axis.line = element_line(colour = "black"),
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 14),
    legend.text = element_text(size = 14),
    legend.title = element_text(size = 14),
    plot.title = element_text(color = "#2F2E41", size = 12, face = "bold"),
    plot.subtitle = element_text(color = "#454543"),
    plot.caption = element_text(color = "#454543", face = "italic")
  )

ggsave("output/model_comm_summary_plot.png", 
       plot = model_comm_summary_plot, 
       device = 'tiff', 
       width = 12, height = 6)

# model-safe
model_safe_summary <- 
  broom.mixed::tidy(model_safe, effects = "fixed", conf.int = TRUE)

model_safe_summary$term <- dplyr::recode(model_safe_summary$term,
                                         "(Intercept)" = "Intercept",
                                         "co_creation_method1" = "Co-creation methods",
                                         "safe_mean_pre_scaled" = "Baseline scores"
)

model_safe_summary_plot <- 
  ggplot(model_safe_summary, 
         aes(x = estimate, 
             y = factor(term, 
                        levels = c("(Intercept)" = "Intercept", 
                                   "co_creation_method1" = "Co-creation methods", 
                                   "comm_mean_pre_scaled" = "Baseline scores")))) +
  geom_point(color = "#205B87", size = 2) +
  geom_errorbar(aes(xmin = conf.low, xmax = conf.high), 
                height = 0.2, color = "#205B87") +
  labs(title = "Frequency of Engagement - Pooled Posterior Estimates",
       subtitle = "95% Credible Intervals (from multiply imputed model)",
       x = "Estimate",
       y = NULL) +
  theme(
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(),
    panel.background = element_blank(), 
    axis.line = element_line(colour = "black"),
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 14),
    legend.text = element_text(size = 14),
    legend.title = element_text(size = 14),
    plot.title = element_text(color = "#2F2E41", size = 12, face = "bold"),
    plot.subtitle = element_text(color = "#454543"),
    plot.caption = element_text(color = "#454543", face = "italic")
  )

ggsave("output/model_safe_summary_plot.png", 
       plot = model_safe_summary_plot, 
       device = 'tiff', 
       width = 12, height = 6)

## ---- compile-model-estimates

# model-freq-change
model_freq_summary_tx <- model_freq_summary
  
model_freq_summary_tx[, c(4, 6, 7)] <- 
  lapply(model_freq_summary_tx[, c(4, 6, 7)], function(x) {(exp(x)-1)*100})

model_freq_summary_tx$variable <- 
  ifelse(model_freq_summary_tx$term == "Co-creation methods", "Total log-ins", NA)

model_freq_summary_tx <- model_freq_summary_tx[!is.na(model_freq_summary_tx$variable), 
                                               c("variable", "estimate", "conf.low", 
                                                 "conf.high")]

# model-intensity-change
model_progress_summary_tx <- model_progress_summary

model_progress_summary_tx[, c(4, 6, 7)] <- 
  lapply(model_progress_summary_tx[, c(4, 6, 7)], 
         function(x) {
           ((withPool_MI(sd_vars["sd_progress"])*x)/
           withPool_MI(mean_v1_vars["mean_progress"]))*100})

model_progress_summary_tx$variable <- 
  ifelse(model_progress_summary_tx$term == "Co-creation methods", "App progress", NA)

model_progress_summary_tx <- model_progress_summary_tx[!is.na(model_progress_summary_tx$variable), 
                                               c("variable", "estimate", "conf.low", 
                                                 "conf.high")]

model_items_summary_tx <- model_items_summary

model_items_summary_tx[, c(4, 6, 7)] <- 
  lapply(model_items_summary_tx[, c(4, 6, 7)], function(x) {(exp(x)-1)*100})

model_items_summary_tx$variable <- 
  ifelse(model_items_summary_tx$term == "Co-creation methods", "Completing open-ended items", NA)

model_items_summary_tx <- model_items_summary_tx[!is.na(model_items_summary_tx$variable), 
                                                       c("variable", "estimate", "conf.low", 
                                                         "conf.high")]

model_act_plan_summary_tx <- model_act_plan_summary

model_act_plan_summary_tx[, c(4, 6, 7)] <- 
  lapply(model_act_plan_summary_tx[, c(4, 6, 7)], function(x) {(exp(x)-1)*100})

model_act_plan_summary_tx$variable <- 
  ifelse(model_act_plan_summary_tx$term == "Co-creation methods", "Chance of completing action plan", NA)

model_act_plan_summary_tx <- model_act_plan_summary_tx[!is.na(model_act_plan_summary_tx$variable), 
                                                 c("variable", "estimate", "conf.low", 
                                                   "conf.high")]

# model-duration-change
model_time_spent_summary_tx <- model_time_spent_summary

model_time_spent_summary_tx[, c(4, 6, 7)] <- 
  lapply(model_time_spent_summary_tx[, c(4, 6, 7)], 
         function(x) {
           ((withPool_MI(sd_vars["sd_average_time_spent"])*x)/
              withPool_MI(mean_v1_vars["mean_average_time_spent"]))*100})

model_time_spent_summary_tx$variable <- 
  ifelse(model_time_spent_summary_tx$term == "Co-creation methods", "Time spent per visit", NA)

model_time_spent_summary_tx <- model_time_spent_summary_tx[!is.na(model_time_spent_summary_tx$variable), 
                                                               c("variable", "estimate", "conf.low", 
                                                                 "conf.high")]

model_days_between_summary_tx <- model_days_between_summary

model_days_between_summary_tx[, c(4, 6, 7)] <- 
  lapply(model_days_between_summary_tx[, c(4, 6, 7)], function(x) {(exp(x)-1)*100})

model_days_between_summary_tx$variable <- 
  ifelse(model_days_between_summary_tx$term == "Co-creation methods", "Total duration of use", NA)

model_days_between_summary_tx <- model_days_between_summary_tx[!is.na(model_days_between_summary_tx$variable), 
                                                       c("variable", "estimate", "conf.low", 
                                                         "conf.high")]

# model-outcomes
model_comm_summary_tx <- model_comm_summary

model_comm_summary_tx[, c(4, 6, 7)] <- 
  lapply(model_comm_summary_tx[, c(4, 6, 7)], 
         function(x) {
           ((withPool_MI(sd_vars["sd_comm_mean_post"])*x)/
              withPool_MI(mean_v1_vars["mean_comm_mean_post"]))*100})

model_comm_summary_tx$variable <- 
  ifelse(model_comm_summary_tx$term == "Co-creation methods", "Communication competency", NA)

model_comm_summary_tx <- model_comm_summary_tx[!is.na(model_comm_summary_tx$variable), 
                                                               c("variable", "estimate", "conf.low", 
                                                                 "conf.high")]

model_safe_summary_tx <- model_safe_summary

model_safe_summary_tx[, c(4, 6, 7)] <- 
  lapply(model_safe_summary_tx[, c(4, 6, 7)], 
         function(x) {
           ((withPool_MI(sd_vars["sd_safe_mean_post"])*x)/
              withPool_MI(mean_v1_vars["mean_safe_mean_post"]))*100})

model_safe_summary_tx$variable <- 
  ifelse(model_safe_summary_tx$term == "Co-creation methods", "Perception of preventable adverse events", NA)

model_safe_summary_tx <- model_safe_summary_tx[!is.na(model_safe_summary_tx$variable), 
                                               c("variable", "estimate", "conf.low", 
                                                 "conf.high")]

impact_all_input <- rbind(model_freq_summary_tx, 
                               model_progress_summary_tx, 
                               model_items_summary_tx, 
                               model_act_plan_summary_tx, 
                               model_time_spent_summary_tx, 
                               model_days_between_summary_tx, 
                               model_comm_summary_tx, 
                               model_safe_summary_tx)

var_labels_impact <- c(
  "Total log-ins" = "Total \nlog-ins",
  "App progress" = "App \nprogress",
  "Completing open-ended items" = "Completing \nopen-ended items",
  "Chance of completing action plan" = "Chance of \ncompleting action \nplan",
  "Time spent per visit" = "Time spent \nper visit",
  "Total duration of use" = "Total duration \nof use",
  "Communication competency" = "Communication \ncompetency",
  "Perception of preventable adverse events" = "Perception of \npreventable adverse \nevents"
)

attach(impact_all_input)

impact_all_input <-
  impact_all_input[order(impact_all_input$estimate, decreasing = FALSE),]

detach(impact_all_input)

impact_all_input_plot <- 
  ggplot(impact_all_input, 
         aes(x = estimate, 
             y = reorder(variable, estimate))) +
  geom_point(color = "#205B87", size = 2) +
  geom_errorbar(aes(xmin = conf.low, xmax = conf.high), 
                height = 0.2, color = "#205B87") +
  labs(title = "Impact of co-creation - Pooled Posterior Estimates",
       subtitle = "95% Credible Intervals (from multiply imputed model)",
       x = "Percentage change between V1 and V2 (%)",
       y = NULL) +
  theme(
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(),
    panel.background = element_blank(), 
    axis.line = element_line(colour = "black"),
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 14),
    legend.text = element_text(size = 14),
    legend.title = element_text(size = 14),
    plot.title = element_text(color = "#2F2E41", size = 12, face = "bold"),
    plot.subtitle = element_text(color = "#454543"),
    plot.caption = element_text(color = "#454543", face = "italic")
  )

ggsave("output/impact_all_input_plot.png", 
       plot = impact_all_input_plot, 
       device = 'tiff', 
       width = 12, height = 6)

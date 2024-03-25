## ---- user-feedback-tables

ux <- user_feedback_var(
  var = "ux", 
  label = "User-friendliness", 
  mu_output = mu_ux, 
  data = data_feedback
)

content <- user_feedback_var(
  var = "content", 
  label = "Content", 
  mu_output = mu_content, 
  data = data_feedback
)

utility <- user_feedback_var(
  var = "utility", 
  label = "Utility", 
  mu_output = mu_utility, 
  data = data_feedback
)

user_feedback_output <- rbind(ux, 
                              content, 
                              utility)

flextable(user_feedback_output) %>% 
  set_header_labels(Variable = "Variable", 
                    Version_one = "Version 1", 
                    Version_two = "Version 2", 
                    U = "U") %>% 
  add_header_row(values = c(" ", 
                            "Median rating", 
                            "Test-statistic"), 
                 colwidths = c(1, 2, 1)) |> 
  align(align = "center", part = "all") %>% 
  add_footer_lines("Note: * p < 0.05, ** p < 0.001") %>% 
  save_as_docx(path = "output/user_feedback_mann_whitney.docx")

## ---- sentiment-analysis-tables
flextable(chi_square_table) %>% 
  add_header_row(values = c(" ", 
                            "Sentiment"), 
                 colwidths = c(1, 3)) |> 
  align(align = "center", part = "all") %>% 
  add_footer_lines(paste0("χ2", 
                          "(", 
                          chi_square_stat$parameter[[1]], 
                          ")", 
                          " = ", 
                          round(chi_square_stat$statistic[[1]], 2), 
                          ", p = .", 
                          signif(chi_square_stat$p.value[[1]], 1))) %>% 
  save_as_docx(path = "output/sentiment_chi_square.docx")

## ---- engagement-metric-tables

apa.reg.table(
  lm_total_log_in, 
  filename = "output/lm_total_log_in.docx",
  table.number = NA, 
  prop.var.conf.level = 0.95
)

apa.reg.table(
  lm_progress_score, 
  filename = "output/lm_progress_score.docx",
  table.number = NA, 
  prop.var.conf.level = 0.95
)

apa.reg.table(
  lm_action_plan, 
  filename = "output/lm_action_plan.docx",
  table.number = NA, 
  prop.var.conf.level = 0.95
)

apa.reg.table(
  lm_average_time_spent, 
  filename = "output/lm_average_time_spent.docx",
  table.number = NA, 
  prop.var.conf.level = 0.95
)

apa.reg.table(
  lm_days_between, 
  filename = "output/lm_days_between.docx",
  table.number = NA, 
  prop.var.conf.level = 0.95
)

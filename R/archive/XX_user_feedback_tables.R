## ---- app-feedback-table

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

kbl(user_feedback_output, 
    col.names = c("Variable", "Version 1", "Version 2", "U"), 
    align = "c") %>% 
  kable_classic() %>%
  add_header_above(c(" " = 1, "Median rating" = 2, "Test-statistic" = 1)) %>% 
  footnote(general = "* p < 0.05, ** p < 0.001")
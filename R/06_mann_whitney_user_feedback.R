## ---- app-feedback-mann-whitney

data_feedback <- final_data_key_vars[
  !duplicated(final_data_key_vars[ ,c('version', 
                                      'UserCode')]),]

mu_ux <- wilcox.test(ux ~ version, 
                     data = data_feedback)

mu_content <- wilcox.test(content ~ version, 
                          data = data_feedback)

mu_utility <- wilcox.test(utility ~ version, 
                          data = data_feedback)
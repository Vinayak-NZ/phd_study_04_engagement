## ---- compare-socio-demographics

# age
app_baseline_valid <- app_baseline_demo[app_baseline_demo$alter > 18, ]

app_baseline_valid_age <- 
  app_baseline_valid[!is.na(app_baseline_valid$alter), ]

t_test_age <- t.test(alter ~ version, 
                     app_baseline_valid_age, 
                     var.equal=TRUE)

# education level
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

chi_square_education <- chisq.test(
  table(app_baseline_valid_education$version, 
        app_baseline_valid_education$ausbildung))

# relationship status
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

chi_square_fam <- chisq.test(
  table(app_baseline_valid_family$version, 
        app_baseline_valid_family$familie))

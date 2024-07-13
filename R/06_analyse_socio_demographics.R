## ---- compare-socio-demographics

# age
t_test_age <- t.test(alter ~ version, 
                     app_baseline_valid_age, 
                     var.equal=TRUE)

# education level
chi_square_education <- chisq.test(
  table(app_baseline_valid_education$version, 
        app_baseline_valid_education$ausbildung))

# relationship status
chi_square_fam <- chisq.test(
  table(app_baseline_valid_family$version, 
        app_baseline_valid_family$familie))

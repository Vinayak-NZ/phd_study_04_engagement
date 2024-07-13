## ---- format-baseline-v2

# v2-baseline-data-01
app_v1_v2_baseline_01 <- 
  app_v1_v2_baseline_01[app_v1_v2_baseline_01$Gruppe_Allgemein == 1, ]

usercode_vars_01 <- 
  app_v1_v2_baseline_01[, grep("Code", 
                               names(app_v1_v2_baseline_01), value = TRUE)]

age_vars_01 <- 
  app_v1_v2_baseline_01[, grep("Alter", 
                            names(app_v1_v2_baseline_01), value = TRUE)]

education_vars_01 <- 
  app_v1_v2_baseline_01[, grep("Ausbildung", 
                            names(app_v1_v2_baseline_01), value = TRUE)]

family_vars_01 <- 
  app_v1_v2_baseline_01[, grep("Familie", 
                            names(app_v1_v2_baseline_01), value = TRUE)]

oe_vars_01 <- 
  app_v1_v2_baseline_01[, grep("OE", 
                               names(app_v1_v2_baseline_01), value = TRUE)]

cse_vars_01 <- 
  app_v1_v2_baseline_01[, grep("CSE", 
                               names(app_v1_v2_baseline_01), value = TRUE)]

int_vars_01 <- 
  app_v1_v2_baseline_01[, grep("Int|INT", 
                               names(app_v1_v2_baseline_01), value = TRUE)]

pl_vars_01 <- 
  app_v1_v2_baseline_01[, grep("PL", 
                               names(app_v1_v2_baseline_01), value = TRUE)]

vueic1_vars_01 <- 
  app_v1_v2_baseline_01[, grep("VUEIC1", 
                               names(app_v1_v2_baseline_01), value = TRUE)]

vueic2_vars_01 <- 
  app_v1_v2_baseline_01[, grep("VUEIC2", 
                               names(app_v1_v2_baseline_01), value = TRUE)]

app_v1_v2_baseline_01$UserCode <- ifelse(app_v1_v2_baseline_01$UserCode_4_Sch_IG < 0 & 
                                        app_v1_v2_baseline_01$Code_schw_KG_ges < 0, 
                                      NA, 
                                      ifelse(app_v1_v2_baseline_01$UserCode_4_Sch_IG < 0 & 
                                               app_v1_v2_baseline_01$Code_schw_KG_ges > 0, 
                                             app_v1_v2_baseline_01$Code_schw_KG_ges, 
                                             app_v1_v2_baseline_01$UserCode_4_Sch_IG))

app_v1_v2_baseline_01$UserCode <- toupper(app_v1_v2_baseline_01$UserCode)

app_v1_v2_baseline_01$alter <- ifelse(app_v1_v2_baseline_01$Alter_Sch_IG < 0 & 
                                        app_v1_v2_baseline_01$Alter_Sch_KG < 0, 
                                   NA, 
                                   ifelse(app_v1_v2_baseline_01$Alter_Sch_IG < 0 & 
                                            app_v1_v2_baseline_01$Alter_Sch_KG > 0, 
                                          app_v1_v2_baseline_01$Alter_Sch_KG, 
                                          app_v1_v2_baseline_01$Alter_Sch_IG))

app_v1_v2_baseline_01$ausbildung <- ifelse(app_v1_v2_baseline_01$Ausbildung_Sch_IG < 0 & 
                                          app_v1_v2_baseline_01$Ausbildung_Sch_KG< 0, 
                                   NA, 
                                   ifelse(app_v1_v2_baseline_01$Ausbildung_Sch_IG < 0 & 
                                            app_v1_v2_baseline_01$Ausbildung_Sch_KG > 0, 
                                          app_v1_v2_baseline_01$Ausbildung_Sch_KG, 
                                          app_v1_v2_baseline_01$Ausbildung_Sch_IG))

app_v1_v2_baseline_01$familie <- ifelse(app_v1_v2_baseline_01$Familie_Sch_IG < 0 & 
                                          app_v1_v2_baseline_01$dupl1_Familie_Sch_IG< 0, 
                                        NA, 
                                        ifelse(app_v1_v2_baseline_01$Familie_Sch_IG < 0 & 
                                                 app_v1_v2_baseline_01$dupl1_Familie_Sch_IG > 0, 
                                               app_v1_v2_baseline_01$dupl1_Familie_Sch_IG, 
                                               app_v1_v2_baseline_01$Familie_Sch_IG))

app_v1_v2_baseline_01$OE <- ifelse(app_v1_v2_baseline_01$OE2_Sch_IG < 0 & 
                                          app_v1_v2_baseline_01$OE2_Schw_KG< 0, 
                                        NA, 
                                        ifelse(app_v1_v2_baseline_01$OE2_Sch_IG < 0 & 
                                                 app_v1_v2_baseline_01$OE2_Schw_KG > 0, 
                                               app_v1_v2_baseline_01$OE2_Schw_KG, 
                                               app_v1_v2_baseline_01$OE2_Sch_IG))

app_v1_v2_baseline_01$CSE <- ifelse(app_v1_v2_baseline_01$CSE2_Sch_IG < 0 & 
                                     app_v1_v2_baseline_01$CSE2_Schw_KG< 0, 
                                   NA, 
                                   ifelse(app_v1_v2_baseline_01$CSE2_Sch_IG < 0 & 
                                            app_v1_v2_baseline_01$CSE2_Schw_KG > 0, 
                                          app_v1_v2_baseline_01$CSE2_Schw_KG, 
                                          app_v1_v2_baseline_01$CSE2_Sch_IG))

app_v1_v2_baseline_01$INT <- ifelse(app_v1_v2_baseline_01$Int_Sch_IG < 0 & 
                                      app_v1_v2_baseline_01$INT_Schw_KG < 0, 
                                    NA, 
                                    ifelse(app_v1_v2_baseline_01$Int_Sch_IG < 0 & 
                                             app_v1_v2_baseline_01$INT_Schw_KG > 0, 
                                           app_v1_v2_baseline_01$INT_Schw_KG, 
                                           app_v1_v2_baseline_01$Int_Sch_IG))

app_v1_v2_baseline_01$PL <- ifelse(app_v1_v2_baseline_01$PL_Sch_IG < 0 & 
                                      app_v1_v2_baseline_01$PL_Schw_KG < 0, 
                                    NA, 
                                    ifelse(app_v1_v2_baseline_01$PL_Sch_IG < 0 & 
                                             app_v1_v2_baseline_01$PL_Schw_KG > 0, 
                                           app_v1_v2_baseline_01$PL_Schw_KG, 
                                           app_v1_v2_baseline_01$PL_Sch_IG))

app_v1_v2_baseline_01$VUEIC1 <- ifelse(app_v1_v2_baseline_01$VUEIC1_Sch_IG < 0 & 
                                     app_v1_v2_baseline_01$VUEIC1_Sch_KG < 0, 
                                   NA, 
                                   ifelse(app_v1_v2_baseline_01$VUEIC1_Sch_IG < 0 & 
                                            app_v1_v2_baseline_01$VUEIC1_Sch_KG > 0, 
                                          app_v1_v2_baseline_01$VUEIC1_Sch_KG, 
                                          app_v1_v2_baseline_01$VUEIC1_Sch_IG))

app_v1_v2_baseline_01$VUEIC2 <- ifelse(app_v1_v2_baseline_01$VUEIC2_Sch_IG < 0 & 
                                         app_v1_v2_baseline_01$VUEIC2_Sch_KG < 0, 
                                       NA, 
                                       ifelse(app_v1_v2_baseline_01$VUEIC2_Sch_IG < 0 & 
                                                app_v1_v2_baseline_01$VUEIC2_Sch_KG > 0, 
                                              app_v1_v2_baseline_01$VUEIC2_Sch_KG, 
                                              app_v1_v2_baseline_01$VUEIC2_Sch_IG))

app_v2_baseline_01 <- 
  app_v1_v2_baseline_01[as.Date(app_v1_v2_baseline_01$datetime) > 
                          as.Date('2022-07-31'), ]

app_v2_baseline_dem_01 <- 
  app_v2_baseline_01[, c("UserCode", 
                         "alter", 
                         "ausbildung", 
                         "familie", 
                         "OE", 
                         "CSE", 
                         "INT", 
                         "PL", 
                         "VUEIC1", 
                         "VUEIC2")]

# v2-baseline-data-02
app_v2_baseline_02 <- 
  app_v2_baseline_02[app_v2_baseline_02$Gruppe_Allgemein == 1, ]

usercode_vars_02 <- 
  app_v2_baseline_02[, grep("Code", 
                               names(app_v2_baseline_02), value = TRUE)]

age_vars_03 <- 
  app_v2_baseline_02[, grep("Alter", 
                               names(app_v2_baseline_02), value = TRUE)]

education_vars_02 <- 
  app_v2_baseline_02[, grep("Ausbildung", 
                               names(app_v2_baseline_02), value = TRUE)]

family_vars_02 <- 
  app_v2_baseline_02[, grep("Familie", 
                               names(app_v2_baseline_02), value = TRUE)]

oe_vars_02 <- 
  app_v2_baseline_02[, grep("OE", 
                               names(app_v2_baseline_02), value = TRUE)]

cse_vars_02 <- 
  app_v2_baseline_02[, grep("CSE", 
                               names(app_v2_baseline_02), value = TRUE)]

int_vars_02<- 
  app_v2_baseline_02[, grep("Int|INT", 
                               names(app_v2_baseline_02), value = TRUE)]

pl_vars_02 <- 
  app_v2_baseline_02[, grep("PL", 
                               names(app_v2_baseline_02), value = TRUE)]

vueic1_vars_02 <- 
  app_v2_baseline_02[, grep("VUEIC1", 
                               names(app_v2_baseline_02), value = TRUE)]

vueic2_vars_02 <- 
  app_v2_baseline_02[, grep("VUEIC2", 
                               names(app_v2_baseline_02), value = TRUE)]

app_v2_baseline_02$UserCode <- app_v2_baseline_02$UserCode_4_Sch_IG

app_v2_baseline_02$UserCode <- toupper(app_v2_baseline_02$UserCode)

app_v2_baseline_02$alter <- app_v2_baseline_02$Alter_Sch_IG

app_v2_baseline_02$ausbildung <- app_v2_baseline_02$Ausbildung_Sch_IG

app_v2_baseline_02$familie <- app_v2_baseline_02$Familie_Sch_IG

app_v2_baseline_02$OE <- app_v2_baseline_02$OE2_Sch_IG

app_v2_baseline_02$CSE <- app_v2_baseline_02$CSE2_Sch_IG

app_v2_baseline_02$INT <- app_v2_baseline_02$Int_Sch_IG

app_v2_baseline_02$PL <- app_v2_baseline_02$PL_Sch_IG

app_v2_baseline_02$VUEIC1 <- app_v2_baseline_02$VUEIC1_Sch_IG

app_v2_baseline_02$VUEIC2 <- app_v2_baseline_02$VUEIC2_Sch_IG

app_v2_baseline_dem_02 <- 
  app_v2_baseline_02[, c("UserCode", 
                         "alter", 
                         "ausbildung", 
                         "familie", 
                         "OE", 
                         "CSE", 
                         "INT", 
                         "PL", 
                         "VUEIC1", 
                         "VUEIC2")]

# v2-baseline-data-03
app_v2_baseline_03 <- 
  app_v2_baseline_03[app_v2_baseline_03$Gruppe_Allgemein == 1, ]

usercode_vars_03 <- 
  app_v2_baseline_03[, grep("Code", 
                            names(app_v2_baseline_03), value = TRUE)]

age_vars_03 <- 
  app_v2_baseline_03[, grep("Alter", 
                            names(app_v2_baseline_03), value = TRUE)]

education_vars_03 <- 
  app_v2_baseline_03[, grep("Ausbildung", 
                            names(app_v2_baseline_03), value = TRUE)]

family_vars_03 <- 
  app_v2_baseline_03[, grep("Familie", 
                            names(app_v2_baseline_03), value = TRUE)]

oe_vars_03 <- 
  app_v2_baseline_03[, grep("OE", 
                            names(app_v2_baseline_03), value = TRUE)]

cse_vars_03 <- 
  app_v2_baseline_03[, grep("CSE", 
                            names(app_v2_baseline_03), value = TRUE)]

int_vars_03 <- 
  app_v2_baseline_03[, grep("Int|INT", 
                            names(app_v2_baseline_03), value = TRUE)]

pl_vars_03 <- 
  app_v2_baseline_03[, grep("PL", 
                            names(app_v2_baseline_03), value = TRUE)]

vueic1_vars_03 <- 
  app_v2_baseline_03[, grep("VUEIC1", 
                            names(app_v2_baseline_03), value = TRUE)]

vueic2_vars_03 <- 
  app_v2_baseline_03[, grep("VUEIC2", 
                            names(app_v2_baseline_03), value = TRUE)]

app_v2_baseline_03$UserCode <- app_v2_baseline_03$UserCode_4_Sch_IG

app_v2_baseline_03$UserCode <- toupper(app_v2_baseline_03$UserCode)

app_v2_baseline_03$alter <- app_v2_baseline_03$Alter_Sch_IG

app_v2_baseline_03$ausbildung <- app_v2_baseline_03$Ausbildung_Sch_IG

app_v2_baseline_03$familie <- app_v2_baseline_03$Familie_Sch_IG

app_v2_baseline_03$OE <- app_v2_baseline_03$OE2_Sch_IG

app_v2_baseline_03$CSE <- app_v2_baseline_03$CSE2_Sch_IG

app_v2_baseline_03$INT <- app_v2_baseline_03$Int_Sch_IG

app_v2_baseline_03$PL <- app_v2_baseline_03$PL_Sch_IG

app_v2_baseline_03$VUEIC1 <- app_v2_baseline_03$VUEIC1_Sch_IG

app_v2_baseline_03$VUEIC2 <- app_v2_baseline_03$VUEIC2_Sch_IG

app_v2_baseline_dem_03 <- 
  app_v2_baseline_03[, c("UserCode", 
                         "alter", 
                         "ausbildung", 
                         "familie", 
                         "OE", 
                         "CSE", 
                         "INT", 
                         "PL", 
                         "VUEIC1", 
                         "VUEIC2")]

# create-single-v2-baseline

app_v2_baseline <- rbind(app_v2_baseline_dem_01, 
                         app_v2_baseline_dem_02, 
                         app_v2_baseline_dem_03)

app_v2_baseline$alter <- ifelse(app_v2_baseline$alter < 0, 
                                   NA, 
                                   app_v2_baseline$alter)

app_v2_baseline$ausbildung <- ifelse(app_v2_baseline$ausbildung < 0, 
                                   NA, 
                                   app_v2_baseline$ausbildung)

app_v2_baseline$familie <- ifelse(app_v2_baseline$familie < 0, 
                                   NA, 
                                   app_v2_baseline$familie)

app_v2_baseline$OE <- ifelse(app_v2_baseline$OE < 0, 
                                   NA, 
                                   app_v2_baseline$OE)

app_v2_baseline$CSE <- ifelse(app_v2_baseline$CSE < 0, 
                                   NA, 
                                   app_v2_baseline$CSE)

app_v2_baseline$INT <- ifelse(app_v2_baseline$INT < 0, 
                                   NA, 
                                   app_v2_baseline$INT)

app_v2_baseline$PL <- ifelse(app_v2_baseline$PL < 0, 
                                   NA, 
                                   app_v2_baseline$PL)

app_v2_baseline$VUEIC1 <- ifelse(app_v2_baseline$VUEIC1 < 0, 
                                   NA, 
                                   app_v2_baseline$VUEIC1)

app_v2_baseline$VUEIC2 <- ifelse(app_v2_baseline$VUEIC2 < 0, 
                                   NA, 
                                   app_v2_baseline$VUEIC2)

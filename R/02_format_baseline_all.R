## ---- baseline-subset
app_v1_baseline_subset <- app_v1_baseline[, c("UUID", 
                                              "OE",
                                              "CSE",
                                              "INT", 
                                              "PL",
                                              "VUEIC1",
                                              "VUEIC2")]

app_v2_baseline_subset <- app_v2_baseline[, c("UserCode", 
                                              "OE",
                                              "CSE",
                                              "INT", 
                                              "PL",
                                              "VUEIC1",
                                              "VUEIC2")]

## ---- baseline-rename-v1
names(app_v1_baseline_subset)[names(app_v1_baseline_subset) == "UUID"] <- "id"

names(app_v1_baseline_subset)[names(app_v1_baseline_subset) == "OE"] <- "hapa2_t0"

names(app_v1_baseline_subset)[names(app_v1_baseline_subset) == "CSE"] <- "hapa3_t0"

names(app_v1_baseline_subset)[names(app_v1_baseline_subset) == "INT"] <- "hapa4_t0"

names(app_v1_baseline_subset)[names(app_v1_baseline_subset) == "PL"] <- "hapa5_t0"

names(app_v1_baseline_subset)[names(app_v1_baseline_subset) == "VUEIC1"] <- "safe1_t0"

names(app_v1_baseline_subset)[names(app_v1_baseline_subset) == "VUEIC2"] <- "safe2_t0"


## ---- baseline-rename-v2
names(app_v2_baseline_subset)[names(app_v2_baseline_subset) == "OE"] <- "hapa2_t0"

names(app_v2_baseline_subset)[names(app_v2_baseline_subset) == "CSE"] <- "hapa3_t0"

names(app_v2_baseline_subset)[names(app_v2_baseline_subset) == "INT"] <- "hapa4_t0"

names(app_v2_baseline_subset)[names(app_v2_baseline_subset) == "PL"] <- "hapa5_t0"

names(app_v2_baseline_subset)[names(app_v2_baseline_subset) == "VUEIC1"] <- "safe1_t0"

names(app_v2_baseline_subset)[names(app_v2_baseline_subset) == "VUEIC2"] <- "safe2_t0"

## ---- combine-baseline-data

app_v1_baseline$version <- "Version 1"

names(app_v1_baseline)[names(app_v1_baseline) == "UUID"] <- "id"

names(app_v1_baseline)[names(app_v1_baseline) == "Alter"] <- "age"

names(app_v1_baseline)[names(app_v1_baseline) == "Ausbildung"] <- "education"

names(app_v1_baseline)[names(app_v1_baseline) == "Familie"] <- "fam_comp"

app_v1_baseline_demo <- app_v1_baseline[, c("version", 
                                              "age",
                                              "education",
                                              "fam_comp")]

app_v1_baseline_demo_j <- app_v1_baseline[, c("id", 
                                              "age",
                                              "education",
                                              "fam_comp")]

app_v2_baseline$version <- "Version 2"

names(app_v2_baseline)[names(app_v2_baseline) == "alter"] <- "age"

names(app_v2_baseline)[names(app_v2_baseline) == "ausbildung"] <- "education"

names(app_v2_baseline)[names(app_v2_baseline) == "familie"] <- "fam_comp"

app_v2_baseline_demo <- app_v2_baseline[, c("version", 
                                            "age",
                                            "education",
                                            "fam_comp")]

app_v2_baseline_demo_j <- app_v2_baseline[, c("UserCode", 
                                            "age",
                                            "education",
                                            "fam_comp")]

app_baseline_demo <- rbind(app_v1_baseline_demo, 
                      app_v2_baseline_demo)

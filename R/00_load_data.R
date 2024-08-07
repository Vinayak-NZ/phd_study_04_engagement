## ---- load-data

app_v1_sentiment <- read.csv("input/app_v1_sentiment_count_df.csv")
app_v1_active_data <- read.csv("input/app_v1_active.csv")
app_v1_passive_data <- read.csv("input/app_v1_passive.csv")
app_v1_item_page_match <- read.csv("input/app_v1_item_page_match.csv", sep=",")
app_v1_page_count <- read.csv("input/app_v1_page_count.csv")
app_v1_clinic <- read.csv("input/app_v1_clinic_participants.csv")
app_v1_baseline <- read.csv("input/app_v1_baseline.csv")

app_v2_sentiment <- read.csv("input/app_v2_sentiment_count_df.csv")
app_v2_active_data <- read.csv("input/app_v2_active.csv")
app_v2_passive_data <- read.csv("input/app_v2_passive.csv")
app_v2_item_page_match <- read.csv("input/app_v2_item_page_match.csv")
app_v2_page_count <- read.csv("input/app_v2_page_count.csv")
app_v1_v2_baseline_01 <- read.csv("input/baseline_v1_v2_improved_german.csv", sep = ";")
app_v2_baseline_02 <- read.csv("input/baseline_v2_improved_nonr_english.csv", sep = ";")
app_v2_baseline_03 <- read.csv("input/baseline_v2_improved_nonr_german.csv", sep = ";")

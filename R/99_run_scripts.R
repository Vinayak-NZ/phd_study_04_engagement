## ---- prep-data
source("R/00_load_data.R")
source("R/00_load_functions.R")
source("R/00_load_package.R")
source("R/01_format_active_data.R")
source("R/01_format_passive_data.R")
source("R/02_format_active_data_feedback.R")
source("R/02_format_active_data_hapa.R")
source("R/02_format_active_data_safety.R")
source("R/02_format_baseline.R")
source("R/02_join_data.R")
source("R/03_prepare_data.R")
source("R/04_derive_variables.R")
source("R/04_process_app_use_data.R")
source("R/04_process_sentiment_data.R")

## ---- analyse-data
source("R/06_chi_square_sentiment.R")
source("R/06_mann_whitney_user_feedback.R")
source("R/06_model_engagement.R")

## ---- output-data
source("R/08_create_output_tables.R")
source("R/09_visualise_engagement_1_frequency.R")
source("R/09_visualise_engagement_2_intensity.R")
source("R/09_visualise_engagement_3_duration.R")
source("R/09_visualise_sentiment.R")
source("R/09_visualise_user_feedback.R")

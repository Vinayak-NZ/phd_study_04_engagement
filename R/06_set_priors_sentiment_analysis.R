## ---- derive-priors-sentiment

app_all_sentiment_input <- setDT(app_all_sentiment)

app_all_sentiment_input <- 
  app_all_sentiment_input[, proportion:=NULL]

app_all_sentiment_input <- dcast(app_all_sentiment_input, 
                                 version ~ sentiment, 
                                 value.var = "count")


app_all_sentiment_input[["other"]] <- 
  app_all_sentiment_input[["neutral"]] +  app_all_sentiment_input[["negative"]]

app_all_sentiment_input[["delta"]] <- 
  app_all_sentiment_input[["positive"]] / app_all_sentiment_input[["other"]]

sentiment_delta_log <- 
  as.numeric(
    log(app_all_sentiment_input[version == "2.0", "delta"] / 
          app_all_sentiment_input[version == "1.0", "delta"]))

sentiment_se <- 
  as.numeric(sqrt(1/app_all_sentiment_input[version == "1.0", "positive"] + 
                    1/app_all_sentiment_input[version == "1.0", "other"] + 
                    1/app_all_sentiment_input[version == "2.0", "positive"] + 
                    1/app_all_sentiment_input[version == "2.0", "other"]))

## ---- set-priors-progress

prior_progress_weight <- 0.5
base_sd_progress <- 0.5

smd <- or2smd(sentiment_delta_log, sentiment_se)

prior_progress_pe <- smd$TE
prior_progress_sd <- smd$seTE

## ---- set-priors-items-completed

prior_items_weight <- 0.5
base_sd_items <- 0.5

prior_items_pe <- sentiment_delta_log
prior_items_sd   <- sentiment_se

## ---- set-priors-action-plan

odds_ratio_action_plan <- exp(sentiment_delta_log)
percent_change_action_plan <- (odds_ratio_action_plan - 1) * 100

prior_action_plan_weight <- 0.5
base_sd_action_plan <- 0.5

prior_action_plan_pe <- sentiment_delta_log
prior_action_plan_sd   <- sentiment_se

## ---- set-priors-time-spent

prior_time_spent_weight <- 0.5
base_sd_time_spent <- 0.5

prior_time_spent_pe <- smd$TE
prior_time_spent_sd <- smd$seTE

## ---- set-priors-comms

prior_comms_weight <- 0.5
base_sd_comms <- 0.5

prior_comms_pe <- smd$TE * prior_comms_weight
prior_comms_sd <- smd$seTE + base_sd_comms

## ---- set-priors-safety

prior_safety_weight <- 0.5
base_sd_safety <- 0.5

prior_safety_pe <- (smd$TE * prior_safety_weight)*-1
prior_safety_sd <- smd$seTE + base_sd_safety

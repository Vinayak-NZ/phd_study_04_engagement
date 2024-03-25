## ---- create-sentiment-matrix

chisquare_input <- matrix(c(app_v1_sentiment$count, 
                            app_v2_sentiment$count), 
                          nrow = 3, 
                          ncol = 2)

## ---- calculate-sentiment-proportions

app_v1_sentiment$proportion <- (app_v1_sentiment$count/sum(app_v1_sentiment$count))*100

app_v2_sentiment$proportion <- (app_v2_sentiment$count/sum(app_v2_sentiment$count))*100

## ---- append-sentiment-data

app_v1_sentiment$version <- rep("1.0", nrow(app_v1_sentiment))

app_v2_sentiment$version <- rep("2.0", nrow(app_v2_sentiment))

app_all_sentiment <- rbind(app_v1_sentiment, app_v2_sentiment)

app_all_sentiment$sentiment <- factor(app_all_sentiment$sentiment, levels = c('positive', 
                                                                              'neutral', 
                                                                              'negative'))
## ---- sentiment-chi-square-table

chi_square_table <- app_all_sentiment[, c("version", "sentiment", "count")]

setDT(chi_square_table)

chi_square_table <- dcast(chi_square_table, version ~ sentiment, value.var = "count")

colnames(chi_square_table)[1] <- "Version"

colnames(chi_square_table)[2] <- "Positive"

colnames(chi_square_table)[3] <- "Neutral"

colnames(chi_square_table)[4] <- "Negative"

chi_square_table <- chi_square_table[, c("Version", "Negative", "Neutral", "Positive")]

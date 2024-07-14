## ---- sentiment-chi-square-test

chi_square_stat <- chisq.test(chisquare_input, 
                              correct = FALSE)

sentiment_chi_square_df <- min(dim(chisquare_input)) - 1

sentiment_chi_square_effect_size <- 
  cramers_V(chi = chi_square_stat$statistic, 
            n = sum(chisquare_input), 
            df = sentiment_chi_square_df)

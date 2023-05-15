
library(dplyr)

extract1 <- rtweet::read_twitter_csv(here::here("Data", "Extracciones Twitter", "E_Twitter_1.csv"))
extract1 <- extract1[extract1$is_retweet==FALSE,]
extract1 <- extract1[,-1]

extract2 <- rtweet::read_twitter_csv(here::here("Data", "Extracciones Twitter", "E_Twitter_2.csv"))
extract2 <- extract2[extract2$is_retweet==FALSE,]
extract2 <- extract2[,-1]

extract3 <- rtweet::read_twitter_csv(here::here("Data", "Extracciones Twitter", "E_Twitter_3.csv"))
extract3 <- extract3[extract3$is_retweet==FALSE,]
extract3 <- extract3[,-1]

extract4 <- rtweet::read_twitter_csv(here::here("Data", "Extracciones Twitter", "E_Twitter_4.csv"))
extract4 <- extract4[extract4$is_retweet==FALSE,]
extract4 <- extract4[,-1]

extract5 <- rtweet::read_twitter_csv(here::here("Data", "Extracciones Twitter", "E_Twitter_5.csv"))
extract5 <- extract5[extract5$is_retweet==FALSE,]
extract5 <- extract5[,-1]

extract6 <- rtweet::read_twitter_csv(here::here("Data", "Extracciones Twitter", "E_Twitter_6.csv"))
extract6 <- extract6[extract6$is_retweet==FALSE,]
extract6 <- extract6[,-1]

extract7 <- rtweet::read_twitter_csv(here::here("Data", "Extracciones Twitter", "E_Twitter_7.csv"))
extract7 <- extract7[extract7$is_retweet==FALSE,]
extract7 <- extract7[,-1]

extract8 <- rtweet::read_twitter_csv(here::here("Data", "Extracciones Twitter", "E_Twitter_8.csv"))
extract8 <- extract8[extract8$is_retweet==FALSE,]
extract8 <- extract8[,-1]

extract9 <- rtweet::read_twitter_csv(here::here("Data", "Extracciones Twitter", "E_Twitter_9.csv"))
extract9 <- extract9[extract9$is_retweet==FALSE,]
extract9 <- extract9[,-1]

extract10 <- rtweet::read_twitter_csv(here::here("Data", "Extracciones Twitter", "E_Twitter_10.csv"))
extract10 <- extract10[extract10$is_retweet==FALSE,]
extract10 <- extract10[,-1]

BaseEx <- full_join(extract1, extract2)
BaseEx <- full_join(BaseEx, extract3)
BaseEx <- full_join(BaseEx, extract4)
BaseEx <- full_join(BaseEx, extract5)
BaseEx <- full_join(BaseEx, extract6)
BaseEx <- full_join(BaseEx, extract7)
BaseEx <- full_join(BaseEx, extract8)
BaseEx <- full_join(BaseEx, extract9)
BaseEx <- full_join(BaseEx, extract10)


Cuentas <- rio::import(here::here("Data", "TWITTER Base Candidatos LA.xlsx"))
Cuentas <- dplyr::select(Cuentas, "Country", "Candidate", "Twitter_account")
names(Cuentas) <- c("Country", "Candidate", "screen_name")
Cuentas <- distinct(Cuentas, screen_name, .keep_all = TRUE)


BaseEx2 <- as.data.frame(BaseEx)
BaseEx2 <- left_join(BaseEx2, Cuentas)


rio::export(BaseEx2, here::here("Data", "Tweets_Candidatos_1.csv"), format = "csv")

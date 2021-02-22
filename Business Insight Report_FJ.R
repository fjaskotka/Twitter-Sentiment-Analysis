################################################
### Created by Fabian Jaskotka on 02/06/2021 ###
########### BUSINESS INSIGHT REPORT  ###########
################################################

#loading necessary libraries
library(dplyr)
library(rtweet)
library(tidytext)
library(tidyverse)
library(stringr)
library(ggplot2)
library(scales)
library(tm)
library(twitteR)

# Change consumer_key, consume_secret, access_token, and 
# access_secret based on your own keys
#consumer_key <- "my_token_1"
#consumer_secret <-"my_token_2"
#access_token <- "my_token_3"
#access_secret <- "my_token_4" 
setup_twitter_oauth(consumer_key, consumer_secret, access_token, access_secret)

################################################################################
######## Importing Data from Twitter - Aviation
################################################################################


##################
# Delta
##################

#downloading Delta related tweets
Delta = searchTwitter('delta airlines -filter:retweets', n = 10000, 
                      resultType = "recent", lang = "en") %>%
          twListToDF()

#transforming into dataset
my_df_1 <- data.frame(Delta, text = Delta$text)

#adding column with search word
my_df_1$company <- "Delta"


##################
# American
##################

#downloading American related tweets
American = searchTwitter('american airlines -filter:retweets', n = 10000, 
                         resultType = "recent", lang = "en") %>%
          twListToDF()

#transforming data into dataset
my_df_2 <- data.frame(American, text = American$text)

#adding column with search word
my_df_2$company <- "American"


##################
# United
##################

#downloading United related tweets
United = searchTwitter('united airlines -filter:retweets', n = 10000, 
                       resultType = "recent", lang = "en") %>%
          twListToDF()

#transforming data into dataset
my_df_3 <- data.frame(United, text = United$text)

#adding column with search word
my_df_3$company <- "United"


################################################################################
######## Tokenizing dataframe
################################################################################

#tokenizing Delta
token_list_1 <- my_df_1 %>%
                  filter(screenName != "laxradar") %>%
                  unnest_tokens(word, text) %>%
                  anti_join(stop_words)

token_1_clean <- token_list_1 %>%
                  count(word,sort = T)

#tokenizing American
token_list_2 <- my_df_2 %>%
                  filter(screenName != "laxradar") %>%
                  unnest_tokens(word, text) %>%
                  anti_join(stop_words)

#tokenizing United
token_list_3 <- my_df_3 %>%
                  filter(screenName != "laxradar") %>%
                  unnest_tokens(word, text) %>%
                  anti_join(stop_words)

################################################################################
####### Plotting Frequency of Delta Words
################################################################################

freq_hist <- token_list_1 %>%
              count(word, sort = TRUE) %>%
              mutate(word = reorder(word,n )) %>%
              filter(n>25) %>%
              filter(word != "delta") %>%
              filter(word != "t.co") %>%
              filter(word != "https") %>%
              filter(word != "airlines") %>%
            ggplot(aes(word, n))+
            geom_col() +
            xlab(NULL) +
            coord_flip()

print(freq_hist)

################################################################################
####### Combining Data and creating Frequency Table
################################################################################

#sorting by frequency
frequency_tokens <-bind_rows(mutate(token_list_1, company="Delta"),
                             mutate(token_list_2, company="American"),
                             mutate(token_list_3, company="United")) %>%
                            mutate(word=str_extract(word, "[a-z']+")) %>%
                            count(company, word) %>%
                            group_by(company) %>%
                            mutate(proportion = n/sum(n))%>%
                            select(-n) %>%
                            spread(company, proportion) %>%
                            gather(company, proportion, `American`, `United`)

print(frequency_tokens)

################################################################################
####### Correlogram
################################################################################

#plotting correlogram
ggplot(frequency_tokens, aes(x=proportion, y=`Delta`, 
                      color = abs(`Delta`- proportion)))+
  geom_abline(color="grey40", lty=2)+
  geom_jitter(alpha=.1, size=2.5, width=0.3, height=0.3)+
  geom_text(aes(label=word), check_overlap = TRUE, vjust=1.5) +
  scale_x_log10(labels = percent_format())+
  scale_y_log10(labels= percent_format())+
  scale_color_gradient(limits = c(0,0.001), low = "darkslategray4", high = "gray75")+
  facet_wrap(~company, ncol=2)+
  theme(legend.position = "none")+
  labs(y= "Delta Airlines", x=NULL)

################################################################################
####### Correlation Test
################################################################################

#correlation between Delta and American
cor.test(data = frequency_tokens[frequency_tokens$company == "American", ],
         ~proportion + `Delta`)

#correlation between Delta and United
cor.test(data = frequency_tokens[frequency_tokens$company == "United", ],
         ~proportion + `Delta`)

################################################################################
####### N-Grams
################################################################################

all_airlines <- bind_rows(mutate(my_df_1, company = "Delta"),
                           mutate(my_df_2, company = "American"),
                           mutate(my_df_3, company = "United")) 

airlines_bigrams <- all_airlines %>%
  unnest_tokens(bigram, text, token = "ngrams", n=3) %>%
  filter(!is.na(bigram)) %>%
  count(company, bigram, sort = TRUE) %>%
  filter(company =="Delta")


################################################################################
####### TF-IDF
################################################################################

#adding data from airlines together
combination <- bind_rows(mutate(token_list_1, company="Delta"),
                         mutate(token_list_2, company="American"),
                         mutate(token_list_3, company="United"))

combination <- combination %>%
  count(company,word, sort = T)

#sorting total words by count per company
total_words <- combination %>%
  group_by(company) %>%
  count(company, word, sort = T) %>%
  summarize(total = sum(n))

company_words <- left_join(combination, total_words)

#adding tf_idf scores to dataset
company_words <- company_words %>%
  bind_tf_idf(word, company, n) %>%
  arrange(desc(tf_idf))

#graphical output
company_words %>%
  arrange(desc(tf_idf)) %>%
  mutate(word = factor(word, levels= rev(unique(word)))) %>%
  group_by(company) %>%
  top_n(15) %>%
  ungroup %>%
  ggplot(aes(word, tf_idf, fill = company)) +
  geom_col(show.legend = F) +
  labs(x =NULL, y = "tf_idf") +
  facet_wrap(~company, ncol=2, scales="free")+
  coord_flip()

################################################################################
####### Sentiment Analysis
################################################################################

# flavor sentiment
token_1_clean %>%
  inner_join(get_sentiments("nrc")) %>%
  count(word, sentiment, sort = T) %>%
  acast(word ~sentiment, value.var = "n", fill=0) %>%
  comparison.cloud(color= c("grey10", "grey60"), 
                   max.words = 200, scale =c(2, 0.1), random.order = T, 
                   title.size = 2)

#binary sentiment
token_1_clean %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = T) %>%
  acast(word ~sentiment, value.var = "n", fill=0) %>%
  comparison.cloud(color= c("grey10", "grey60"), 
                   max.words = 200, scale =c(3, 0.1), random.order = T, 
                   title.size = 2)
        
